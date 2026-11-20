#' Contiguous clusters of majority-group blocks
#'
#' Within each place, finds groups of adjacent blocks in which the group is a
#' majority and reports the largest. Set against the population of an ideal
#' council district, the largest cluster is a census-based proxy for whether
#' a majority-group district could be drawn.
#'
#' @inheritParams seg_measures
#' @param blocks An sf object with one row per block, such as the output of
#'   [seg_blocks()].
#' @param threshold A block qualifies when the group's share of its
#'   population is strictly above `threshold`. The default, 0.5, means a
#'   majority.
#' @param n_districts <[`data-masking`][rlang::args_data_masking]> The number
#'   of districts each unit would be divided into: a single number, or a
#'   column of `blocks` that is constant within each unit. `NULL` (the
#'   default) leaves out the district columns.
#' @param bridge_empty Should blocks with no population join clusters? The
#'   default, `TRUE`, lets them; `FALSE` is a robustness setting. See
#'   Details.
#' @param contiguity `"rook"` (the default) treats blocks as adjacent when
#'   they share an edge; `"queen"` also counts a shared corner.
#'
#' @details
#' Clusters are the connected components of the adjacency graph among
#' qualifying blocks in a unit. Blocks in different units never join. Rook
#' contiguity matches the usual legal standard for districts.
#'
#' Blocks with no population, such as parks, freeway land, or industrial
#' parcels, have no group share. With `bridge_empty = TRUE` (the default)
#' they enter the graph, so two clusters separated only by empty blocks count
#' as one. A district can include zero-population blocks at no cost in
#' population equality, so a mapmaker could join the two clusters in one
#' contiguous district. Empty blocks add nothing to a cluster's population,
#' and clusters made only of empty blocks are dropped. With
#' `bridge_empty = FALSE` empty blocks are left out and can split clusters;
#' use it as a robustness check, since it can change whether a place has a
#' cluster of district size. Either way, every populated block in a cluster
#' is above `threshold`, so the cluster as a whole is too.
#'
#' The ideal district population is the unit's population divided by
#' `n_districts`. Two columns count clusters against it:
#'
#' * `n_viable` is the number of clusters with at least `ideal_pop` people.
#'   A cluster four times the ideal size counts once.
#' * `n_districts_floor` is the sum over clusters of
#'   `floor(cluster population / ideal_pop)`. Any union of majority-group
#'   blocks is itself majority-group, so this approximates how many
#'   majority-group districts could be drawn from majority-group blocks
#'   alone. It is a heuristic, not a bound in either direction: a cluster
#'   cannot always be split into contiguous pieces of exactly equal
#'   population, and real districts can add blocks where the group is a
#'   minority and still have a group majority.
#'
#' Adjacency is found with [sf::st_relate()] in the coordinates of `blocks`.
#' For longitude and latitude this treats coordinates as planar, which does
#' not change whether two blocks share a boundary.
#'
#' @return A tibble with one row per unit: the `by` columns, `pop_total`,
#'   `n_clusters`, and the largest cluster's population (`largest_pop`),
#'   group population (`largest_group`), and group share (`largest_share`,
#'   `NA` when there is no cluster). With `n_districts`, it also has
#'   `n_districts`, `ideal_pop`, `largest_ratio` (`largest_pop / ideal_pop`),
#'   `n_viable`, and `n_districts_floor` (see Details).
#' @seealso [seg_cluster_blocks()] for each block's cluster, and
#'   [seg_measures()] for aspatial measures.
#' @export
#' @examples
#' # A row of five blocks; the middle one is empty.
#' blocks <- sf::st_sf(
#'   place = "a",
#'   latino = c(60, 70, 0, 80, 10),
#'   total = c(100, 100, 0, 100, 100),
#'   geometry = sf::st_make_grid(cellsize = 1, offset = c(0, 0), n = c(5, 1))
#' )
#' seg_clusters(blocks, by = place, group = latino, total = total,
#'              n_districts = 2)
#' seg_clusters(blocks, by = place, group = latino, total = total,
#'              n_districts = 2, bridge_empty = FALSE)
seg_clusters <- function(blocks, by, group, total, threshold = 0.5,
                         n_districts = NULL, bridge_empty = TRUE,
                         contiguity = c("rook", "queen")) {
  check_sf(blocks)
  check_number(threshold, \(x) x >= 0 && x < 1, "a number in [0, 1)")
  check_flag(bridge_empty)
  contiguity <- rlang::arg_match(contiguity)
  by_cols <- resolve_cols(rlang::enquo(by), blocks, "by")
  data <- prepare_counts(blocks, by_cols, {{ group }}, {{ total }})

  has_districts <- !rlang::quo_is_null(rlang::enquo(n_districts))
  if (has_districts) {
    data <- dplyr::mutate(data, .n_districts = {{ n_districts }})
    check_districts(data, by_cols)
  }

  units <- split_units(data, by_cols)
  summaries <- lapply(units$rows, \(i) {
    unit <- data[i, ]
    labels <- label_clusters(unit, threshold, contiguity, bridge_empty)
    summarize_clusters(unit, labels)
  })
  dplyr::bind_cols(units$keys, dplyr::bind_rows(summaries))
}

#' Label each block with its majority-group cluster
#'
#' Runs the same cluster search as [seg_clusters()] and returns the blocks
#' with each one's cluster, for mapping or for checking a place by hand.
#'
#' @inheritParams seg_clusters
#' @return `blocks` without the rows that have a missing `by` value, plus an
#'   integer column `cluster`: 1 for the unit's most populous cluster, 2 for
#'   the next, and so on, and `NA` for blocks in no cluster. Empty blocks
#'   that bridge clusters carry the label of the cluster they join.
#' @seealso [seg_clusters()] for one summary row per unit.
#' @export
#' @examples
#' blocks <- sf::st_sf(
#'   place = "a",
#'   latino = c(60, 70, 0, 80, 10),
#'   total = c(100, 100, 0, 100, 100),
#'   geometry = sf::st_make_grid(cellsize = 1, offset = c(0, 0), n = c(5, 1))
#' )
#' seg_cluster_blocks(blocks, by = place, group = latino, total = total)
seg_cluster_blocks <- function(blocks, by, group, total, threshold = 0.5,
                               bridge_empty = TRUE,
                               contiguity = c("rook", "queen")) {
  check_sf(blocks)
  check_number(threshold, \(x) x >= 0 && x < 1, "a number in [0, 1)")
  check_flag(bridge_empty)
  contiguity <- rlang::arg_match(contiguity)
  by_cols <- resolve_cols(rlang::enquo(by), blocks, "by")
  data <- prepare_counts(blocks, by_cols, {{ group }}, {{ total }})

  cluster <- rep(NA_integer_, nrow(data))
  for (i in split_units(data, by_cols)$rows) {
    cluster[i] <- label_clusters(data[i, ], threshold, contiguity, bridge_empty)
  }
  data$cluster <- cluster
  data[, setdiff(names(data), c(".group", ".total"))]
}

check_districts <- function(data, by_cols, call = rlang::caller_env()) {
  n <- data$.n_districts
  if (!is.numeric(n) || anyNA(n) || any(n < 1) || !all(is_whole(n))) {
    cli::cli_abort(
      "{.arg n_districts} must be a positive whole number or a column of them.",
      call = call
    )
  }
  n_varying <- drop_geometry(data) |>
    dplyr::summarise(k = dplyr::n_distinct(.data$.n_districts),
                     .by = dplyr::all_of(by_cols)) |>
    dplyr::filter(.data$k > 1) |>
    nrow()
  if (n_varying > 0) {
    cli::cli_abort(
      "{.arg n_districts} must be constant within each unit; {n_varying}
       unit{?s} ha{?s/ve} more than one value.",
      call = call
    )
  }
  invisible()
}

# Row indices of each unit, and one row of `by` keys per unit.
split_units <- function(data, by_cols) {
  keys <- drop_geometry(data)[by_cols]
  unit_id <- dplyr::group_indices(
    dplyr::group_by(keys, dplyr::across(dplyr::everything()))
  )
  rows <- split(seq_len(nrow(data)), unit_id)
  first_rows <- vapply(rows, `[`, integer(1), 1)
  list(rows = rows, keys = dplyr::as_tibble(keys[first_rows, , drop = FALSE]))
}

# Cluster label for each block of one unit: 1 for the most populous cluster,
# NA for blocks outside every cluster and for clusters with no population.
label_clusters <- function(unit, threshold, contiguity, bridge_empty) {
  qualifies <- unit$.total > 0 & unit$.group > threshold * unit$.total
  in_graph <- which(qualifies | (bridge_empty & unit$.total == 0))
  labels <- rep(NA_integer_, nrow(unit))
  if (length(in_graph) == 0) {
    return(labels)
  }

  pattern <- c(rook = "F***1****", queen = "F***T****")[[contiguity]]
  # st_relate() messages that lon/lat coordinates are treated as planar;
  # that does not affect whether two blocks share a boundary.
  neighbors <- suppressMessages(sf::st_relate(unit[in_graph, ], pattern = pattern))
  graph <- igraph::graph_from_adj_list(unclass(neighbors), mode = "all")
  membership <- igraph::components(graph)$membership

  pop <- tapply(unit$.total[in_graph], membership, sum)
  populated <- as.integer(names(pop))[pop > 0]
  ranked <- populated[order(pop[pop > 0], decreasing = TRUE)]
  labels[in_graph] <- match(membership, ranked)
  labels
}

summarize_clusters <- function(unit, labels) {
  pop <- as.numeric(tapply(unit$.total, labels, sum))
  group <- as.numeric(tapply(unit$.group, labels, sum))
  found <- length(pop) > 0

  out <- dplyr::tibble(
    pop_total = sum(unit$.total),
    n_clusters = length(pop),
    largest_pop = if (found) pop[1] else 0,
    largest_group = if (found) group[1] else 0,
    largest_share = if (found) group[1] / pop[1] else NA_real_
  )
  if (".n_districts" %in% names(unit)) {
    n_districts <- unit$.n_districts[1]
    out$n_districts <- n_districts
    out$ideal_pop <- out$pop_total / n_districts
    out$largest_ratio <- out$largest_pop / out$ideal_pop
    # Compare pop * n_districts with pop_total so that a cluster of exactly
    # the ideal size is not lost to rounding.
    out$n_viable <- sum(pop * n_districts >= out$pop_total)
    out$n_districts_floor <- sum((pop * n_districts) %/% out$pop_total)
  }
  out
}
