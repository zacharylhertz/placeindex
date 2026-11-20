#' Aspatial concentration measures for places
#'
#' Summarizes how a group is spread across the census blocks of each place,
#' or of any other unit. Every measure compares the group with everyone else
#' in the unit (the "rest").
#'
#' @param blocks A data frame or sf object with one row per block, such as
#'   the output of [seg_blocks()]. Geometry, if present, is ignored.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select]> Column or columns
#'   identifying the unit, such as `place_geoid`. Blocks with a missing value
#'   in any `by` column are dropped.
#' @param group,total <[`data-masking`][rlang::args_data_masking]> Block
#'   counts of the group and of the whole population. `group` must not exceed
#'   `total`.
#'
#' @details
#' For a unit with blocks \eqn{i = 1, \dots, n}, write \eqn{g_i} for the group
#' count, \eqn{t_i} for the total, \eqn{r_i = t_i - g_i} for the rest, and
#' \eqn{G} and \eqn{R} for the sums of \eqn{g_i} and \eqn{r_i}.
#'
#' * `group_share` is the group's share of the unit's population.
#' * `dissimilarity` is \eqn{\frac{1}{2} \sum_i |g_i / G - r_i / R|}: the
#'   share of the group that would have to move to another block for every
#'   block to match the unit's composition.
#' * `isolation` is \eqn{\sum_i (g_i / G)(g_i / t_i)}: the group's share of
#'   the block the average group member lives in.
#' * `exposure` is \eqn{\sum_i (g_i / G)(r_i / t_i)}: the rest's share of the
#'   block the average group member lives in. It equals `1 - isolation`.
#' * `majority_share` is the share of the group living in blocks where it is
#'   more than half of the population.
#'
#' Blocks with no population add nothing to any measure. A measure that
#' divides by \eqn{G} or \eqn{R} is `NA` when that sum is zero.
#'
#' @return A tibble with one row per unit: the `by` columns, `n_blocks`,
#'   `pop_total`, `pop_group`, `group_share`, `dissimilarity`, `isolation`,
#'   `exposure`, and `majority_share`.
#' @seealso [seg_clusters()] for a spatial measure.
#' @export
#' @examples
#' blocks <- data.frame(
#'   place = c("a", "a", "a", "b", "b"),
#'   latino = c(40, 10, 0, 5, 5),
#'   total = c(50, 50, 20, 10, 10)
#' )
#' seg_measures(blocks, by = place, group = latino, total = total)
seg_measures <- function(blocks, by, group, total) {
  check_data_frame(blocks)
  by_cols <- resolve_cols(rlang::enquo(by), blocks, "by")
  data <- prepare_counts(drop_geometry(blocks), by_cols, {{ group }}, {{ total }})

  data |>
    dplyr::summarise(
      concentration_summary(.data$.group, .data$.total),
      .by = dplyr::all_of(by_cols)
    ) |>
    dplyr::as_tibble()
}

# Add `.group` and `.total`, drop blocks outside any unit, and validate.
prepare_counts <- function(data, by_cols, group, total,
                           call = rlang::caller_env()) {
  data <- data |>
    dplyr::mutate(.group = {{ group }}, .total = {{ total }}) |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(by_cols), is.na))
  check_counts(data$.group, data$.total, call = call)
  data
}

concentration_summary <- function(g, t) {
  r <- t - g
  G <- sum(g)
  R <- sum(r)
  dplyr::tibble(
    n_blocks = length(g),
    pop_total = sum(t),
    pop_group = G,
    group_share = if (sum(t) > 0) G / sum(t) else NA_real_,
    dissimilarity = if (G > 0 && R > 0) sum(abs(g / G - r / R)) / 2 else NA_real_,
    isolation = interaction_index(g, g, t),
    exposure = interaction_index(g, r, t),
    majority_share = if (G > 0) sum(g[g > t / 2]) / G else NA_real_
  )
}

# The share of `x` in the block of the average group member.
interaction_index <- function(g, x, t) {
  G <- sum(g)
  if (G == 0) {
    return(NA_real_)
  }
  lived_in <- t > 0
  sum(g[lived_in] / G * x[lived_in] / t[lived_in])
}
