#' Population-weighted centroids of larger units
#'
#' Places one point in each larger unit, such as a census tract, at the
#' population-weighted mean location of its blocks. These points are the
#' usual origins for access measures. A tract's geometric center can fall in
#' empty land; its population-weighted centroid falls where people live.
#'
#' @param blocks An sf object of small units, usually census blocks, with
#'   point or polygon geometry. Each polygon is first replaced by a point
#'   inside it ([sf::st_point_on_surface()]). Passing points is faster: the
#'   2020 TIGER/Line block files carry an internal point for each block
#'   (`INTPTLON20`, `INTPTLAT20`).
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select]> One column naming
#'   the larger unit each block belongs to. For 2020 blocks the tract is the
#'   first 11 characters of the block GEOID.
#' @param weight <[`tidy-select`][dplyr::dplyr_tidy_select]> One numeric
#'   column of non-negative block weights, usually total population.
#'
#' @details
#' The centroid of unit \eqn{u} is the weighted mean
#' \deqn{(\bar x_u, \bar y_u) = \frac{\sum_{b \in u} w_b (x_b, y_b)}{\sum_{b \in u} w_b}.}
#' A unit whose blocks all have zero weight, such as a tract of parkland or
#' water, gets the unweighted mean of its block points instead, and a message
#' counts such units.
#'
#' Means are taken in the coordinates of `blocks`, so project to a planar
#' CRS first, such as California Albers (EPSG:3310). With longitude and
#' latitude the mean is a planar approximation: close within a tract, poor
#' across a large unit.
#'
#' @return An sf object of points, one row per unit sorted by unit, with the
#'   `by` column, the summed `weight` column, `n_blocks`, and the CRS of
#'   `blocks`.
#' @seealso [acc_cost_matrix()], which takes these points as origins.
#' @export
#' @examples
#' blocks <- sf::st_as_sf(
#'   data.frame(
#'     tract = c("t1", "t1", "t2"),
#'     pop = c(1, 3, 5),
#'     x = c(0, 4, 10),
#'     y = c(0, 0, 10)
#'   ),
#'   coords = c("x", "y"),
#'   crs = 3310
#' )
#' # t1 sits at (0 * 1 + 4 * 3) / 4 = 3 on the x axis.
#' acc_centroids(blocks, tract, pop)
acc_centroids <- function(blocks, by, weight) {
  check_sf(blocks)
  by_col <- single_col(rlang::enquo(by), blocks, "by")
  weight_col <- single_col(rlang::enquo(weight), blocks, "weight")
  attrs <- sf::st_drop_geometry(blocks)
  unit <- attrs[[by_col]]
  w <- attrs[[weight_col]]
  if (anyNA(unit)) {
    cli::cli_abort("{.arg by} must not be missing for any block.")
  }
  check_nonnegative(w, "weight")
  xy <- point_coordinates(blocks, "blocks", polygons_ok = TRUE)

  ids <- unique(unit)
  g <- match(unit, ids)
  sum_by <- function(x) rowsum(x, g, reorder = FALSE)[, 1]
  w_sum <- sum_by(w)
  n <- sum_by(rep(1, length(g)))
  zero <- w_sum == 0
  x <- ifelse(zero, sum_by(xy[, 1]) / n, sum_by(w * xy[, 1]) / w_sum)
  y <- ifelse(zero, sum_by(xy[, 2]) / n, sum_by(w * xy[, 2]) / w_sum)
  if (any(zero)) {
    cli::cli_inform(
      "{sum(zero)} unit{?s} ha{?s/ve} zero total {.arg weight} and
       {?gets/get} the unweighted mean of {?its/their} block points."
    )
  }

  out <- dplyr::tibble(
    !!by_col := ids,
    !!weight_col := unname(w_sum),
    n_blocks = as.integer(n),
    .x = unname(x),
    .y = unname(y)
  )
  out <- out[order(ids), ]
  sf::st_as_sf(out, coords = c(".x", ".y"), crs = sf::st_crs(blocks))
}

#' Origin-destination costs
#'
#' Builds the long table of travel costs between origins (such as tract
#' centroids from [acc_centroids()]) and sites (such as library outlets),
#' either as straight-line distance or from a table you supply, such as
#' tract-to-tract travel times from OpenTimes (<https://opentimes.org>).
#'
#' @param from,to Origins and destinations: data frames or sf objects with
#'   one row per unit. For `method = "euclidean"` both must be sf points in
#'   the same projected CRS.
#' @param from_id,to_id <[`tidy-select`][dplyr::dplyr_tidy_select]> One
#'   column each of unique, non-missing IDs.
#' @param method `"euclidean"` for straight-line distance in the units of the
#'   CRS (meters for EPSG:3310), or `"provided"` to validate and filter the
#'   table passed as `cost`.
#' @param cost For `method = "provided"`, a data frame with columns `from`,
#'   `to` and `cost`. Rename the columns of a travel-time table to these
#'   first. Must be `NULL` for `method = "euclidean"`.
#' @param max_cost Optional positive number. Pairs costing more are dropped,
#'   which keeps the table small. Set it to at least the catchment you will
#'   pass to [acc_e2sfca()].
#'
#' @details
#' Euclidean costs are computed in blocks of origins, about five million
#' distances at a time, so the full origin-by-site matrix is never held in
#' memory. The returned table still has one row per pair, so set `max_cost`
#' to keep it small in a large run.
#'
#' For a provided table, `acc_cost_matrix()` checks the three columns, rejects
#' missing or negative costs and repeated origin-destination pairs, and drops
#' rows whose IDs are not in `from` or `to`, with a message counting them
#' (a national travel-time file covers far more tracts than one state). If no
#' row matches, the IDs probably differ in type or format, such as numeric
#' GEOIDs that have lost a leading zero, and the function stops. A pair
#' missing from the table counts as out of reach.
#'
#' @return A tibble with columns `from`, `to` and `cost`, sorted by origin
#'   and then destination in the order of `from` and `to`.
#' @seealso [acc_e2sfca()].
#' @export
#' @examples
#' origins <- sf::st_as_sf(
#'   data.frame(id = c("a", "b", "c"), x = c(0, 3, 6), y = c(0, 4, 8)),
#'   coords = c("x", "y"), crs = 3310
#' )
#' sites <- sf::st_as_sf(
#'   data.frame(site = c("x", "y"), x = c(0, 6), y = c(0, 0)),
#'   coords = c("x", "y"), crs = 3310
#' )
#' acc_cost_matrix(origins, sites, id, site)
#' acc_cost_matrix(origins, sites, id, site, max_cost = 6)
#'
#' # A provided table, such as travel times in minutes.
#' times <- data.frame(from = c("a", "b", "z"), to = c("x", "y", "x"),
#'                     cost = c(5, 12, 3))
#' acc_cost_matrix(origins, sites, id, site, method = "provided", cost = times)
acc_cost_matrix <- function(from, to, from_id, to_id,
                            method = c("euclidean", "provided"),
                            cost = NULL, max_cost = NULL) {
  check_data_frame(from)
  check_data_frame(to)
  method <- rlang::arg_match(method)
  from_ids <- unit_ids(rlang::enquo(from_id), from, "from_id")
  to_ids <- unit_ids(rlang::enquo(to_id), to, "to_id")
  if (!is.null(max_cost)) {
    check_number(max_cost, \(x) x > 0, "a positive number or `NULL`")
  }

  if (method == "euclidean") {
    if (!is.null(cost)) {
      cli::cli_abort('{.arg cost} is only used when {.code method = "provided"}.')
    }
    return(euclidean_costs(from, to, from_ids, to_ids, max_cost))
  }

  if (is.null(cost)) {
    cli::cli_abort('{.code method = "provided"} needs a {.arg cost} table.')
  }
  cost <- check_cost_table(cost)
  drop <- !(cost$from %in% from_ids) | !(cost$to %in% to_ids)
  if (all(drop)) {
    cli::cli_abort(c(
      "No row of {.arg cost} has an origin in {.arg from} and a destination
       in {.arg to}.",
      i = "Check that the IDs share a type and format, such as character
           GEOIDs with leading zeros."
    ))
  }
  if (any(drop)) {
    cli::cli_inform(
      "Dropped {sum(drop)} row{?s} of {.arg cost} whose origin or destination
       is not in {.arg from} or {.arg to}."
    )
  }
  cost <- cost[!drop, ]
  if (!is.null(max_cost)) {
    cost <- cost[cost$cost <= max_cost, ]
  }
  cost[order(match(cost$from, from_ids), match(cost$to, to_ids)), ]
}

euclidean_costs <- function(from, to, from_ids, to_ids, max_cost,
                            call = rlang::caller_env()) {
  check_sf(from, "from", call = call)
  check_sf(to, "to", call = call)
  if (sf::st_crs(from) != sf::st_crs(to)) {
    cli::cli_abort("{.arg from} and {.arg to} must share a CRS.", call = call)
  }
  if (isTRUE(sf::st_is_longlat(from))) {
    cli::cli_abort(c(
      "Euclidean costs need a projected CRS, not longitude and latitude.",
      i = "Transform both with {.fn sf::st_transform}, for example to
           California Albers (EPSG:3310)."
    ), call = call)
  }
  fxy <- point_coordinates(from, "from", call = call)
  txy <- point_coordinates(to, "to", call = call)

  chunk <- max(1L, floor(5e6 / max(nrow(txy), 1L)))
  starts <- seq(1L, nrow(fxy), by = chunk)
  pieces <- lapply(starts, function(s) {
    i <- s:min(s + chunk - 1L, nrow(fxy))
    d <- sqrt(outer(fxy[i, 1], txy[, 1], "-")^2 +
                outer(fxy[i, 2], txy[, 2], "-")^2)
    keep <- if (is.null(max_cost)) {
      which(!is.na(d), arr.ind = TRUE)
    } else {
      which(d <= max_cost, arr.ind = TRUE)
    }
    data.frame(f = i[keep[, 1]], t = keep[, 2], cost = d[keep])
  })
  pairs <- do.call(rbind, pieces)
  pairs <- pairs[order(pairs$f, pairs$t), ]
  dplyr::tibble(
    from = from_ids[pairs$f],
    to = to_ids[pairs$t],
    cost = pairs$cost
  )
}

#' Enhanced two-step floating catchment area (E2SFCA) access
#'
#' Scores each origin's access to sites with limited capacity, such as
#' library hours or clinic doctors, allowing for the people at other origins
#' who compete for the same sites.
#'
#' @param demand A data frame or sf object with one row per origin.
#' @param supply A data frame or sf object with one row per site.
#' @param cost A table with columns `from` (origin ID), `to` (site ID) and
#'   `cost`, usually from [acc_cost_matrix()]. A pair missing from the table
#'   is out of reach.
#' @param demand_id,supply_id <[`tidy-select`][dplyr::dplyr_tidy_select]> One
#'   column each of unique IDs matching `cost$from` and `cost$to`.
#' @param population <[`tidy-select`][dplyr::dplyr_tidy_select]> Column of
#'   `demand` with each origin's population (non-negative).
#' @param capacity <[`tidy-select`][dplyr::dplyr_tidy_select]> Column of
#'   `supply` with each site's capacity (non-negative).
#' @param catchment The largest cost at which a site is in reach, in the
#'   units of `cost`.
#' @param decay How the weight falls with cost:
#'   * `"gaussian"` (default): \eqn{W(d) = (e^{-(d/d_0)^2/2} - e^{-1/2}) /
#'     (1 - e^{-1/2})} for \eqn{d \le d_0}, the catchment. The weight is 1
#'     at zero cost and falls smoothly to 0 at the catchment edge (the form
#'     used by Dai 2010).
#'   * `"step"`: a constant weight within each band of `bands`.
#' @param bands For `decay = "step"`, a data frame with columns `upper`
#'   (positive, strictly increasing upper bounds, the last equal to
#'   `catchment`) and `weight` (between 0 and 1). A cost \eqn{d} falls in the
#'   first band whose `upper` is at least \eqn{d}. `NULL` uses three equal
#'   bands with weights 1, 0.68 and 0.22, as in Luo and Qi (2009).
#'
#' @details
#' With populations \eqn{P_i}, capacities \eqn{S_j}, costs \eqn{d_{ij}} and
#' decay weights \eqn{W}:
#'
#' 1. Each site's ratio of capacity to the decay-weighted population in its
#'    catchment: \deqn{R_j = \frac{S_j}{\sum_{k: d_{kj} \le d_0} P_k W(d_{kj})}.}
#' 2. Each origin's access, the decay-weighted sum of the ratios of sites in
#'    reach: \deqn{A_i = \sum_{j: d_{ij} \le d_0} R_j W(d_{ij}).}
#'
#' Access is in units of capacity per person, such as annual library hours
#' per resident. Because the same weights enter both steps,
#' \eqn{\sum_i P_i A_i = \sum_j S_j} over the sites that have anyone in reach:
#' the measure shares out all the supply. A site with no population in reach
#' has an undefined ratio; it is set to 0 so that it adds nothing to any
#' origin. An origin with no site in reach gets access 0.
#'
#' Rows of `cost` whose IDs are not in `demand` or `supply` are ignored, with
#' a message. If no row matches, the IDs probably differ in type or format,
#' and the function stops rather than return zero access everywhere.
#'
#' @return A tibble with one row per row of `demand`, in the same order: the
#'   `demand_id` column, `access` and `n_sites` (the number of sites in reach
#'   with positive weight).
#' @references
#' Luo, W., and Qi, Y. (2009). An enhanced two-step floating catchment area
#' (E2SFCA) method for measuring spatial accessibility to primary care
#' physicians. *Health & Place*, 15(4), 1100-1107.
#'
#' Dai, D. (2010). Black residential segregation, disparities in spatial
#' access to health care facilities, and late-stage breast cancer diagnosis
#' in metropolitan Detroit. *Health & Place*, 16(5), 1038-1052.
#' @seealso [acc_cost_matrix()], [acc_centroids()].
#' @export
#' @examples
#' demand <- data.frame(id = c("a", "b", "c"), pop = c(100, 200, 300))
#' supply <- data.frame(site = c("x", "y"), hours = c(10, 30))
#' cost <- data.frame(
#'   from = c("a", "a", "b", "b", "c", "c"),
#'   to = c("x", "y", "x", "y", "x", "y"),
#'   cost = c(0, 20, 8, 4, 15, 0)
#' )
#' acc_e2sfca(demand, supply, cost, id, pop, site, hours, catchment = 10)
#' acc_e2sfca(demand, supply, cost, id, pop, site, hours, catchment = 10,
#'            decay = "step",
#'            bands = data.frame(upper = c(5, 10), weight = c(1, 0.5)))
acc_e2sfca <- function(demand, supply, cost, demand_id, population,
                       supply_id, capacity, catchment,
                       decay = c("gaussian", "step"), bands = NULL) {
  check_data_frame(demand)
  check_data_frame(supply)
  cost <- check_cost_table(cost)
  decay <- rlang::arg_match(decay)
  check_number(catchment, \(x) x > 0 && is.finite(x), "a positive number")
  bands <- check_bands(bands, decay, catchment)

  d_name <- single_col(rlang::enquo(demand_id), demand, "demand_id")
  d_ids <- unit_ids(rlang::enquo(demand_id), demand, "demand_id")
  s_ids <- unit_ids(rlang::enquo(supply_id), supply, "supply_id")
  pop <- drop_geometry(demand)[[single_col(rlang::enquo(population), demand,
                                           "population")]]
  cap <- drop_geometry(supply)[[single_col(rlang::enquo(capacity), supply,
                                           "capacity")]]
  check_nonnegative(pop, "population")
  check_nonnegative(cap, "capacity")

  fi <- match(cost$from, d_ids)
  ti <- match(cost$to, s_ids)
  unknown <- is.na(fi) | is.na(ti)
  if (length(unknown) > 0 && all(unknown)) {
    cli::cli_abort(c(
      "No row of {.arg cost} has an origin in {.arg demand} and a site in
       {.arg supply}.",
      i = "Check that the IDs share a type and format, such as character
           GEOIDs with leading zeros."
    ))
  }
  if (any(unknown)) {
    cli::cli_inform(
      "Ignoring {sum(unknown)} row{?s} of {.arg cost} whose origin or site is
       not in {.arg demand} or {.arg supply}."
    )
  }
  w <- decay_weight(cost$cost, catchment, decay, bands)
  keep <- !unknown & w > 0
  fi <- fi[keep]
  ti <- ti[keep]
  w <- w[keep]

  site_demand <- group_sum(pop[fi] * w, ti, length(s_ids))
  ratio <- ifelse(site_demand > 0, cap / site_demand, 0)
  dplyr::tibble(
    !!d_name := d_ids,
    access = group_sum(ratio[ti] * w, fi, length(d_ids)),
    n_sites = tabulate(fi, nbins = length(d_ids))
  )
}

decay_weight <- function(d, catchment, decay, bands) {
  w <- if (decay == "gaussian") {
    k <- exp(-0.5)
    (exp(-0.5 * (d / catchment)^2) - k) / (1 - k)
  } else {
    band <- findInterval(d, bands$upper, left.open = TRUE) + 1L
    bands$weight[pmin(band, nrow(bands))]
  }
  w[d > catchment] <- 0
  w
}

check_bands <- function(bands, decay, catchment, call = rlang::caller_env()) {
  if (decay == "gaussian") {
    if (!is.null(bands)) {
      cli::cli_abort('{.arg bands} is only used when {.code decay = "step"}.',
                     call = call)
    }
    return(NULL)
  }
  if (is.null(bands)) {
    return(data.frame(upper = catchment * (1:3) / 3, weight = c(1, 0.68, 0.22)))
  }
  check_data_frame(bands, "bands", call = call)
  if (!all(c("upper", "weight") %in% names(bands))) {
    cli::cli_abort("{.arg bands} must have columns {.field upper} and
                    {.field weight}.", call = call)
  }
  upper <- bands$upper
  weight <- bands$weight
  if (!is.numeric(upper) || !is.numeric(weight) || length(upper) == 0 ||
      anyNA(upper) || anyNA(weight)) {
    cli::cli_abort("{.field upper} and {.field weight} in {.arg bands} must be
                    numeric and non-missing.", call = call)
  }
  if (upper[1] <= 0 || any(diff(upper) <= 0)) {
    cli::cli_abort("{.field upper} in {.arg bands} must be positive and
                    strictly increasing.", call = call)
  }
  if (!isTRUE(all.equal(upper[length(upper)], catchment))) {
    cli::cli_abort("The last {.field upper} in {.arg bands} must equal
                    {.arg catchment} ({catchment}).", call = call)
  }
  if (any(weight < 0 | weight > 1)) {
    cli::cli_abort("{.field weight} in {.arg bands} must be between 0 and 1.",
                   call = call)
  }
  data.frame(upper = upper, weight = weight)
}

check_cost_table <- function(cost, call = rlang::caller_env()) {
  check_data_frame(cost, "cost", call = call)
  missing <- setdiff(c("from", "to", "cost"), names(cost))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg cost} must have columns {.field from}, {.field to} and
       {.field cost}; {.field {missing}} {?is/are} missing.",
      i = "Rename a travel-time table first, for example with
           {.code dplyr::rename(from = origin_id, to = destination_id,
           cost = duration_sec)}."
    ), call = call)
  }
  x <- dplyr::as_tibble(drop_geometry(cost))[c("from", "to", "cost")]
  if (!is.numeric(x$cost) || anyNA(x$cost) || any(x$cost < 0)) {
    cli::cli_abort("{.field cost} in {.arg cost} must be numeric, non-missing
                    and non-negative.", call = call)
  }
  if (anyNA(x$from) || anyNA(x$to)) {
    cli::cli_abort("{.field from} and {.field to} in {.arg cost} must not be
                    missing.", call = call)
  }
  if (dplyr::n_distinct(x$from, x$to) < nrow(x)) {
    cli::cli_abort("{.arg cost} repeats at least one {.field from}-{.field to}
                    pair; keep one cost per pair.", call = call)
  }
  x
}

# Resolve a tidy-select argument that must name exactly one column.
single_col <- function(quo, data, arg, call = rlang::caller_env()) {
  if (rlang::quo_is_missing(quo)) {
    cli::cli_abort("{.arg {arg}} is required.", call = call)
  }
  col <- resolve_cols(quo, data, arg, call = call)
  if (length(col) != 1) {
    cli::cli_abort("{.arg {arg}} must select exactly one column.", call = call)
  }
  col
}

unit_ids <- function(quo, data, arg, call = rlang::caller_env()) {
  ids <- drop_geometry(data)[[single_col(quo, data, arg, call = call)]]
  if (anyNA(ids)) {
    cli::cli_abort("{.arg {arg}} must not have missing values.", call = call)
  }
  n_dup <- sum(duplicated(ids))
  if (n_dup > 0) {
    cli::cli_abort("{.arg {arg}} must be unique; {n_dup} value{?s}
                    {?is/are} repeated.", call = call)
  }
  ids
}

check_nonnegative <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || anyNA(x) || any(x < 0)) {
    cli::cli_abort("{.arg {arg}} must be numeric, non-missing and
                    non-negative.", call = call)
  }
  invisible(x)
}

# Coordinates of point geometries (or points on the surface of polygons).
point_coordinates <- function(x, arg, polygons_ok = FALSE,
                              call = rlang::caller_env()) {
  geom <- sf::st_geometry(x)
  n_empty <- sum(sf::st_is_empty(geom))
  if (n_empty > 0) {
    cli::cli_abort("{.arg {arg}} has {n_empty} empty geometr{?y/ies}.",
                   call = call)
  }
  type <- as.character(sf::st_geometry_type(geom, by_geometry = TRUE))
  if (polygons_ok && all(type %in% c("POLYGON", "MULTIPOLYGON"))) {
    geom <- sf::st_point_on_surface(geom)
  } else if (!all(type == "POINT")) {
    cli::cli_abort(c(
      "{.arg {arg}} must have POINT geometry.",
      i = "Use {.fn acc_centroids} or {.fn sf::st_point_on_surface} first."
    ), call = call)
  }
  sf::st_coordinates(geom)[, 1:2, drop = FALSE]
}

group_sum <- function(x, g, n) {
  out <- numeric(n)
  if (length(g) > 0) {
    s <- rowsum(x, g)
    out[as.integer(rownames(s))] <- s[, 1]
  }
  out
}
