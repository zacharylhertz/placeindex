# Internal helpers shared across the function families.

drop_geometry <- function(x) {
  if (inherits(x, "sf")) sf::st_drop_geometry(x) else x
}

# Resolve a tidy-select argument (passed as a quosure) to column names.
resolve_cols <- function(cols, data, arg, call = rlang::caller_env()) {
  pos <- tidyselect::eval_select(
    cols,
    drop_geometry(data),
    allow_rename = FALSE,
    error_call = call
  )
  if (length(pos) == 0) {
    cli::cli_abort("{.arg {arg}} must select at least one column.", call = call)
  }
  names(pos)
}

check_data_frame <- function(x, arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a data frame, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
  invisible(x)
}

check_sf <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!inherits(x, "sf")) {
    cli::cli_abort(
      "{.arg {arg}} must be an sf object, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
  invisible(x)
}

check_flag <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!rlang::is_bool(x)) {
    cli::cli_abort("{.arg {arg}} must be `TRUE` or `FALSE`.", call = call)
  }
  invisible(x)
}

check_string <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!rlang::is_string(x) || !nzchar(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-empty string.", call = call)
  }
  invisible(x)
}

# Check a single number against `test`; `must` completes "`arg` must be ...".
check_number <- function(x, test, must, arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  ok <- is.numeric(x) && length(x) == 1 && !is.na(x) && test(x)
  if (!ok) {
    cli::cli_abort("{.arg {arg}} must be {must}.", call = call)
  }
  invisible(x)
}

is_whole <- function(x) x == round(x)

# Block counts must be complete, non-negative, and have group <= total.
check_counts <- function(group, total, call = rlang::caller_env()) {
  if (!is.numeric(group) || !is.numeric(total)) {
    cli::cli_abort("{.arg group} and {.arg total} must be numeric.", call = call)
  }
  if (anyNA(group) || anyNA(total)) {
    cli::cli_abort(
      "{.arg group} and {.arg total} must not contain missing values.",
      call = call
    )
  }
  if (any(group < 0)) {
    cli::cli_abort("{.arg group} must not be negative.", call = call)
  }
  n_over <- sum(group > total)
  if (n_over > 0) {
    cli::cli_abort(
      "{n_over} block{?s} ha{?s/ve} a {.arg group} count above {.arg total}.",
      call = call
    )
  }
  invisible()
}

# Indicator columns must be numeric, and (for normalizing) must vary.
check_indicators <- function(data, col_names, varying = FALSE,
                             call = rlang::caller_env()) {
  values <- drop_geometry(data)[col_names]
  not_numeric <- col_names[!vapply(values, is.numeric, logical(1))]
  if (length(not_numeric) > 0) {
    cli::cli_abort(
      "Indicator columns must be numeric; {.field {not_numeric}} {?is/are} not.",
      call = call
    )
  }
  if (varying) {
    n_distinct <- vapply(values, \(x) length(unique(x[!is.na(x)])), integer(1))
    constant <- col_names[n_distinct < 2]
    if (length(constant) > 0) {
      cli::cli_abort(
        "{.field {constant}} ha{?s/ve} fewer than two distinct values and
         cannot be normalized.",
        call = call
      )
    }
  }
  invisible()
}

indicator_matrix <- function(data, col_names) {
  as.matrix(drop_geometry(data)[col_names])
}
