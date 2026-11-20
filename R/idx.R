#' Normalize indicators to a common scale
#'
#' Puts indicators measured in different units on a common scale so they can
#' be combined with [idx_combine()].
#'
#' @param data A data frame or sf object with one row per unit.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Numeric indicator
#'   columns. They are replaced in place.
#' @param method One of:
#'   * `"z"`: subtract the mean and divide by the standard deviation.
#'   * `"minmax"`: rescale linearly from 0 (lowest) to 1 (highest).
#'   * `"rank"`: percentile rank, \eqn{(r - 1) / (n - 1)}, where tied values
#'     share the lowest rank \eqn{r} (see [dplyr::percent_rank()]).
#'
#' @details
#' Missing values stay missing and are ignored when computing means, ranges,
#' and ranks. A column with fewer than two distinct values says nothing about
#' how units differ, so it is an error rather than a column of zeros.
#'
#' Every method keeps the direction of the indicator: higher stays higher.
#' Reverse an indicator before normalizing if lower values should count
#' toward a higher index.
#'
#' @return `data` with the `cols` columns normalized.
#' @seealso [idx_combine()], [idx_sensitivity()].
#' @export
#' @examples
#' cities <- data.frame(
#'   city = c("a", "b", "c", "d"),
#'   share = c(0.2, 0.5, 0.6, 0.9),
#'   isolation = c(0.3, 0.4, 0.8, 0.7)
#' )
#' idx_normalize(cities, c(share, isolation), method = "minmax")
#' idx_normalize(cities, c(share, isolation), method = "rank")
idx_normalize <- function(data, cols, method = c("z", "minmax", "rank")) {
  check_data_frame(data)
  method <- rlang::arg_match(method)
  col_names <- resolve_cols(rlang::enquo(cols), data, "cols")
  check_indicators(data, col_names, varying = TRUE)

  dplyr::mutate(
    data,
    dplyr::across(dplyr::all_of(col_names), \(x) normalize_vector(x, method))
  )
}

normalize_vector <- function(x, method) {
  switch(method,
    z = (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE),
    minmax = (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)),
    rank = dplyr::percent_rank(x)
  )
}

#' Combine indicators into a composite index
#'
#' Takes a weighted arithmetic or geometric mean of indicator columns,
#' usually after [idx_normalize()].
#'
#' @inheritParams idx_normalize
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Numeric indicator
#'   columns to combine.
#' @param weights Optional non-negative weights, one per selected column,
#'   either in the order selected or named by column. They are rescaled to
#'   sum to 1. `NULL` (the default) weights every column equally.
#' @param method `"arithmetic"` for the weighted mean \eqn{\sum_j w_j x_j},
#'   or `"geometric"` for the weighted geometric mean
#'   \eqn{\prod_j x_j^{w_j}}. Under the geometric mean a high value on one
#'   indicator makes up less for a low value on another.
#' @param name Name of the new index column.
#'
#' @details
#' The geometric mean is defined only for positive values. `idx_combine()`
#' does not shift or truncate values to make them positive, because the size
#' of any shift changes the result. It stops with an error that counts the
#' values at or below zero. Min-max and rank normalization both set the
#' lowest unit to 0, so rescale to a positive range, such as 1 to 100, before
#' asking for a geometric mean.
#'
#' A unit missing any indicator gets a missing index.
#'
#' @return `data` with a new column named `name`.
#' @seealso [idx_normalize()], [idx_sensitivity()].
#' @export
#' @examples
#' cities <- data.frame(
#'   city = c("a", "b", "c", "d"),
#'   share = c(0.2, 0.5, 0.6, 0.9),
#'   isolation = c(0.3, 0.4, 0.8, 0.7)
#' )
#' cities |>
#'   idx_normalize(c(share, isolation), method = "z") |>
#'   idx_combine(c(share, isolation), weights = c(share = 2, isolation = 1))
#'
#' idx_combine(cities, c(share, isolation), method = "geometric")
idx_combine <- function(data, cols, weights = NULL,
                        method = c("arithmetic", "geometric"), name = "index") {
  check_data_frame(data)
  method <- rlang::arg_match(method)
  check_string(name)
  col_names <- resolve_cols(rlang::enquo(cols), data, "cols")
  check_indicators(data, col_names)

  values <- indicator_matrix(data, col_names)
  weights <- standardize_weights(weights, col_names)
  if (method == "geometric") {
    check_positive(values)
  }
  data[[name]] <- aggregate_index(values, weights, method)
  data
}

standardize_weights <- function(weights, col_names, call = rlang::caller_env()) {
  k <- length(col_names)
  if (is.null(weights)) {
    return(rep(1 / k, k))
  }
  valid <- is.numeric(weights) && length(weights) == k && !anyNA(weights) &&
    all(weights >= 0) && sum(weights) > 0
  if (!valid) {
    cli::cli_abort(
      "{.arg weights} must be {k} non-negative number{?s} with a positive sum.",
      call = call
    )
  }
  if (!is.null(names(weights))) {
    if (!setequal(names(weights), col_names)) {
      cli::cli_abort(
        "The names of {.arg weights} must match the selected columns:
         {.field {col_names}}.",
        call = call
      )
    }
    weights <- weights[col_names]
  }
  unname(weights / sum(weights))
}

check_positive <- function(values, call = rlang::caller_env()) {
  n_bad <- sum(values <= 0, na.rm = TRUE)
  if (n_bad > 0) {
    cli::cli_abort(
      c(
        "The geometric mean needs positive values.",
        "x" = "Found {n_bad} value{?s} at or below zero.",
        "i" = "Rescale the indicators to a positive range first."
      ),
      call = call
    )
  }
  invisible()
}

aggregate_index <- function(values, weights, method) {
  switch(method,
    arithmetic = drop(values %*% weights),
    geometric = exp(drop(log(values) %*% weights))
  )
}

#' Sensitivity of index ranks to construction choices
#'
#' Builds the index many times, each time drawing a normalization, a
#' weighting scheme, and an aggregation method at random, and reports how far
#' each unit's rank moves.
#'
#' @inheritParams idx_combine
#' @param id <[`tidy-select`][dplyr::dplyr_tidy_select]> The column that
#'   identifies units. Its values must be unique.
#' @param normalizations Normalization methods to draw from; see
#'   [idx_normalize()].
#' @param weightings Weighting schemes to draw from: `"equal"`, or
#'   `"dirichlet"` for weights drawn from a symmetric Dirichlet distribution
#'   with parameter `concentration`.
#' @param aggregations Aggregation methods to draw from; see [idx_combine()].
#' @param n_sims Number of draws.
#' @param seed Optional whole number. With a seed the draws are reproducible,
#'   and the session's random number stream is restored afterward.
#' @param concentration Parameter of the Dirichlet distribution. The default,
#'   1, draws uniformly from all weight vectors that sum to 1; larger values
#'   keep draws closer to equal weights.
#' @param level Coverage of the rank interval.
#' @param keep_draws If `TRUE`, also return every draw.
#'
#' @details
#' In each draw every option is equally likely, independently of the other
#' choices and of other draws. Rank 1 is the highest index value, and ties
#' share the average rank.
#'
#' Geometric aggregation needs positive inputs (see [idx_combine()]), so in
#' draws that use it each normalized indicator is first rescaled linearly to
#' run from 1 to 100. The 100:1 ratio of the endpoints sets how hard the
#' geometric mean penalizes a low value; the absolute scale does not affect
#' ranks. Because z-scores and min-max values are both linear in the raw
#' data, they give identical geometric draws; only rank normalization gives
#' different ones.
#'
#' @return If `keep_draws = FALSE`, a tibble with one row per unit: the `id`
#'   column, `median_rank`, and `rank_lower` and `rank_upper`, the quantiles
#'   bounding the central `level` share of the unit's ranks. If
#'   `keep_draws = TRUE`, a list with that tibble as `summary` and a tibble
#'   `draws` with one row per draw and unit: `draw`, the `id` column,
#'   `normalization`, `weighting`, `aggregation`, `index`, and `rank`.
#' @seealso [idx_normalize()], [idx_combine()].
#' @export
#' @examples
#' cities <- data.frame(
#'   city = c("a", "b", "c", "d"),
#'   share = c(0.2, 0.5, 0.6, 0.9),
#'   isolation = c(0.3, 0.4, 0.8, 0.7)
#' )
#' idx_sensitivity(cities, c(share, isolation), id = city,
#'                 n_sims = 200, seed = 1)
idx_sensitivity <- function(data, cols, id,
                            normalizations = c("z", "minmax", "rank"),
                            weightings = c("equal", "dirichlet"),
                            aggregations = c("arithmetic", "geometric"),
                            n_sims = 1000, seed = NULL, concentration = 1,
                            level = 0.9, keep_draws = FALSE) {
  check_data_frame(data)
  col_names <- resolve_cols(rlang::enquo(cols), data, "cols")
  id_name <- resolve_cols(rlang::enquo(id), data, "id")
  if (length(id_name) != 1) {
    cli::cli_abort("{.arg id} must select exactly one column.")
  }
  ids <- drop_geometry(data)[[id_name]]
  if (anyNA(ids) || anyDuplicated(ids) > 0) {
    cli::cli_abort("{.arg id} must have unique, non-missing values.")
  }
  check_indicators(data, col_names, varying = TRUE)
  normalizations <- rlang::arg_match(
    normalizations, c("z", "minmax", "rank"), multiple = TRUE
  )
  weightings <- rlang::arg_match(
    weightings, c("equal", "dirichlet"), multiple = TRUE
  )
  aggregations <- rlang::arg_match(
    aggregations, c("arithmetic", "geometric"), multiple = TRUE
  )
  check_number(n_sims, \(x) x >= 1 && is_whole(x), "a positive whole number")
  if (!is.null(seed)) {
    check_number(seed, is_whole, "a whole number or `NULL`")
  }
  check_number(concentration, \(x) x > 0, "a positive number")
  check_number(level, \(x) x > 0 && x < 1, "a number between 0 and 1")
  check_flag(keep_draws)

  # Normalizing is deterministic, so prepare each input matrix once.
  normalized <- lapply(rlang::set_names(normalizations), \(method) {
    normalized_data <- idx_normalize(data, dplyr::all_of(col_names), method)
    indicator_matrix(normalized_data, col_names)
  })
  inputs <- list(
    arithmetic = normalized,
    geometric = lapply(normalized, rescale_columns, to = c(1, 100))
  )

  simulate <- \() lapply(seq_len(n_sims), \(draw) {
    normalization <- pick_one(normalizations)
    weighting <- pick_one(weightings)
    aggregation <- pick_one(aggregations)
    weights <- switch(weighting,
      equal = rep(1 / length(col_names), length(col_names)),
      dirichlet = draw_dirichlet(length(col_names), concentration)
    )
    index <- aggregate_index(inputs[[aggregation]][[normalization]], weights,
                             aggregation)
    dplyr::tibble(
      draw = draw,
      !!id_name := ids,
      normalization = normalization,
      weighting = weighting,
      aggregation = aggregation,
      index = index,
      rank = rank(-index, na.last = "keep", ties.method = "average")
    )
  })
  # with_seed() restores the session's random number stream afterward.
  draws <- if (is.null(seed)) simulate() else withr::with_seed(seed, simulate())
  draws <- dplyr::bind_rows(draws)

  outside <- (1 - level) / 2
  summary <- draws |>
    dplyr::summarise(
      median_rank = stats::median(.data$rank, na.rm = TRUE),
      rank_lower = stats::quantile(.data$rank, outside, na.rm = TRUE,
                                   names = FALSE),
      rank_upper = stats::quantile(.data$rank, 1 - outside, na.rm = TRUE,
                                   names = FALSE),
      .by = dplyr::all_of(id_name)
    )

  if (keep_draws) list(summary = summary, draws = draws) else summary
}

pick_one <- function(x) x[sample.int(length(x), 1)]

draw_dirichlet <- function(k, concentration) {
  gammas <- stats::rgamma(k, shape = concentration)
  gammas / sum(gammas)
}

rescale_columns <- function(values, to) {
  low <- apply(values, 2, min, na.rm = TRUE)
  high <- apply(values, 2, max, na.rm = TRUE)
  scaled <- sweep(sweep(values, 2, low), 2, high - low, "/")
  to[1] + scaled * diff(to)
}
