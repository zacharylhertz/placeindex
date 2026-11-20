#' Weighted aggregation of unit scores to larger units
#'
#' Rolls scores for small units, such as census tracts, up to larger units,
#' such as counties, as a weighted mean. Survey data often identify only the
#' larger unit, so an index built for tracts has to be aggregated before it
#' can be linked to respondents.
#'
#' @param data A data frame or sf object of small units. Geometry is dropped.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more columns
#'   naming the larger unit each row belongs to. For census tracts the county
#'   is the first five characters of the tract GEOID.
#' @param value <[`tidy-select`][dplyr::dplyr_tidy_select]> One numeric
#'   column of scores to aggregate.
#' @param weight <[`tidy-select`][dplyr::dplyr_tidy_select]> One numeric
#'   column of non-negative weights, usually the population the score is
#'   for.
#' @param na_rm If `FALSE` (the default), a group with any missing score gets
#'   a missing value. If `TRUE`, units with missing scores are left out, and
#'   the weight and unit count describe only the units used.
#'
#' @details
#' The value for group \eqn{g} is \eqn{\sum_{i \in g} w_i v_i / \sum_{i \in g} w_i}.
#' A group whose weights sum to zero has no defined mean; it gets `NA` and a
#' message counts such groups. Groups with missing values in `by` are kept as
#' their own group, as in [dplyr::group_by()].
#'
#' @return A tibble with one row per group: the `by` columns, the weighted
#'   mean (named after `value`), the total weight (named after `weight`) and
#'   `n_units`, the number of units in the group.
#' @export
#' @examples
#' tracts <- data.frame(
#'   county = c("a", "a", "a", "b", "b"),
#'   access = c(10, 20, 40, 5, 15),
#'   youth = c(100, 100, 200, 300, 100)
#' )
#' val_aggregate(tracts, county, access, youth)
val_aggregate <- function(data, by, value, weight, na_rm = FALSE) {
  check_data_frame(data)
  check_flag(na_rm)
  df <- drop_geometry(data)
  by_cols <- resolve_cols(rlang::enquo(by), df, "by")
  value_col <- single_col(rlang::enquo(value), df, "value")
  weight_col <- single_col(rlang::enquo(weight), df, "weight")
  if (any(c(value_col, weight_col) %in% by_cols) || value_col == weight_col) {
    cli::cli_abort(
      "{.arg by}, {.arg value} and {.arg weight} must name different columns."
    )
  }
  v <- df[[value_col]]
  if (!is.numeric(v)) {
    cli::cli_abort("{.arg value} must be numeric, not {.obj_type_friendly {v}}.")
  }
  w <- df[[weight_col]]
  check_nonnegative(w, "weight")

  # Units left out by `na_rm` get zero weight and are not counted.
  used <- if (na_rm) !is.na(v) else rep(TRUE, length(v))
  out <- df[by_cols] |>
    dplyr::mutate(.v = ifelse(used, v, 0), .w = ifelse(used, w, 0),
                  .u = used) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) |>
    dplyr::summarise(
      .weight = sum(.data$.w),
      .value = sum(.data$.w * .data$.v) / sum(.data$.w),
      n_units = sum(.data$.u),
      .groups = "drop"
    )
  zero <- !is.na(out$.weight) & out$.weight == 0
  if (any(zero)) {
    cli::cli_inform(
      "{sum(zero)} group{?s} ha{?s/ve} zero total weight; {?its/their}
       value is {.val NA}."
    )
    out$.value[zero] <- NA_real_
  }
  out |>
    dplyr::select(dplyr::all_of(by_cols), ".value", ".weight", "n_units") |>
    dplyr::rename(!!value_col := ".value", !!weight_col := ".weight")
}

#' Shrunken group effects from a multilevel model
#'
#' Extracts each group's random intercept from a multilevel model fitted
#' with lme4, with its conditional standard error, an interval, the number of
#' observations in the group and a flag for intervals that exclude zero. Used
#' to find the groups, such as counties, where an outcome is higher or lower
#' than the model's fixed part predicts.
#'
#' @param model A model fitted with [lme4::lmer()] or [lme4::glmer()] (class
#'   `merMod`) that has a random intercept for `group`.
#' @param group The name of a grouping factor in `model`, bare or quoted, as
#'   it appears in the random-effects term, for example `county` in
#'   `(1 | county)`.
#' @param level Coverage of the interval, between 0 and 1.
#'
#' @details
#' A multilevel model gives each group its own intercept, drawn from a common
#' normal distribution whose spread the model estimates. The intercepts
#' returned here are the conditional modes ([lme4::ranef()]): each group's raw
#' deviation pulled toward zero, more strongly when the group has few
#' observations or the groups differ little. This pull is called shrinkage.
#'
#' `std_error` is the conditional standard deviation from
#' `lme4::ranef(condVar = TRUE)`. It treats the fixed effects and the
#' variance of the intercepts as known, so the intervals are somewhat too
#' narrow. For [lme4::glmer()] fits the effects are on the scale of the link
#' function, such as log odds for a logistic model.
#'
#' The flag `excludes_zero` is a screening device, not a test. With many
#' groups, some intervals will exclude zero by chance, so raise `level` when
#' screening many groups.
#'
#' @return A tibble with one row per group, sorted from the largest effect to
#'   the smallest: the group (in a column named after `group`), `estimate`,
#'   `std_error`, `lower`, `upper`, `n` (observations used in the fit) and
#'   `excludes_zero`.
#' @export
#' @examplesIf rlang::is_installed("lme4")
#' fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
#' val_group_effects(fit, Subject)
val_group_effects <- function(model, group, level = 0.9) {
  rlang::check_installed("lme4", reason = "to extract random effects.")
  if (!inherits(model, "merMod")) {
    cli::cli_abort(
      "{.arg model} must be fitted with {.pkg lme4} (class {.cls merMod}),
       not {.obj_type_friendly {model}}."
    )
  }
  if (rlang::quo_is_missing(rlang::enquo(group))) {
    cli::cli_abort("{.arg group} is required.")
  }
  group <- rlang::as_name(rlang::enquo(group))
  check_number(level, \(x) x > 0 && x < 1, "a number between 0 and 1")

  flist <- lme4::getME(model, "flist")
  if (!group %in% names(flist)) {
    cli::cli_abort(c(
      "{.val {group}} is not a grouping factor in {.arg model}.",
      i = "Grouping factors: {.val {names(flist)}}."
    ))
  }
  re <- as.data.frame(lme4::ranef(model, condVar = TRUE))
  re <- re[re$grpvar == group & re$term == "(Intercept)", , drop = FALSE]
  if (nrow(re) == 0) {
    cli::cli_abort("{.arg model} has no random intercept for {.val {group}}.")
  }
  n <- table(flist[[group]])
  z <- stats::qnorm(1 - (1 - level) / 2)
  out <- dplyr::tibble(
    !!group := as.character(re$grp),
    estimate = re$condval,
    std_error = re$condsd,
    lower = re$condval - z * re$condsd,
    upper = re$condval + z * re$condsd,
    n = as.integer(n[as.character(re$grp)]),
    excludes_zero = re$condval - z * re$condsd > 0 |
      re$condval + z * re$condsd < 0
  )
  dplyr::arrange(out, dplyr::desc(.data$estimate))
}
