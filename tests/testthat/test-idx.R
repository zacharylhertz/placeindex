test_that("idx_normalize applies each method", {
  data <- indicator_fixture()

  z <- idx_normalize(data, c(a, b), method = "z")
  expect_equal(z$a, as.vector(scale(data$a)))
  expect_equal(z$unit, data$unit)

  # a = (1, 2, 3, 6) has range 5.
  minmax <- idx_normalize(data, a, method = "minmax")
  expect_equal(minmax$a, c(0, 0.2, 0.4, 1))
  expect_equal(minmax$b, data$b)

  # b = (4, 1, 2, 8) ranks 3, 1, 2, 4; percentile rank is (rank - 1) / 3.
  rank <- idx_normalize(data, b, method = "rank")
  expect_equal(rank$b, c(2, 0, 1, 3) / 3)
})

test_that("idx_normalize keeps missing values and rejects constants", {
  data <- indicator_fixture()
  data$a[2] <- NA
  expect_equal(idx_normalize(data, a, "minmax")$a, c(0, NA, 0.4, 1))

  data$b <- 1
  expect_error(idx_normalize(data, b), "distinct")
  expect_error(idx_normalize(data, unit), "numeric")
})

test_that("idx_combine takes weighted arithmetic and geometric means", {
  data <- indicator_fixture()

  equal <- idx_combine(data, c(a, b))
  expect_equal(equal$index, c(2.5, 1.5, 2.5, 7))

  # Named weights in any order, rescaled to 0.75 and 0.25.
  weighted <- idx_combine(data, c(a, b), weights = c(b = 1, a = 3), name = "score")
  expect_equal(weighted$score, 0.75 * data$a + 0.25 * data$b)

  geometric <- idx_combine(data, c(a, b), method = "geometric")
  expect_equal(geometric$index, sqrt(data$a * data$b))
})

test_that("idx_combine handles non-positive and missing values", {
  data <- indicator_fixture()
  data$a[1] <- 0
  expect_error(idx_combine(data, c(a, b), method = "geometric"), "positive")

  data$a[1] <- NA
  expect_true(is.na(idx_combine(data, c(a, b))$index[1]))
  expect_error(idx_combine(data, c(a, b), weights = 1), "weights")
  expect_error(idx_combine(data, c(a, b), weights = c(a = 1, c = 1)), "names")
})

test_that("idx_sensitivity returns one row per unit and is reproducible", {
  data <- indicator_fixture()
  first <- idx_sensitivity(data, c(a, b), id = unit, n_sims = 200, seed = 42)
  second <- idx_sensitivity(data, c(a, b), id = unit, n_sims = 200, seed = 42)

  expect_identical(first, second)
  expect_named(first, c("unit", "median_rank", "rank_lower", "rank_upper"))
  expect_equal(first$unit, data$unit)
  expect_true(all(first$rank_lower <= first$median_rank))
  expect_true(all(first$median_rank <= first$rank_upper))
  expect_true(all(first$rank_lower >= 1 & first$rank_upper <= 4))
})

test_that("idx_sensitivity leaves the session's random numbers alone", {
  data <- indicator_fixture()
  set.seed(10)
  expected <- stats::runif(1)
  set.seed(10)
  idx_sensitivity(data, c(a, b), id = unit, n_sims = 20, seed = 99)
  expect_equal(stats::runif(1), expected)
})

test_that("a unit ahead on every indicator always ranks first", {
  # Every normalization, weighting, and aggregation preserves this order.
  data <- data.frame(unit = c("w", "x", "y", "z"), a = c(4, 3, 2, 1),
                     b = c(40, 30, 20, 10))
  out <- idx_sensitivity(data, c(a, b), id = unit, n_sims = 100, seed = 1)
  expect_equal(out$median_rank, 1:4)
  expect_equal(out$rank_lower, 1:4)
  expect_equal(out$rank_upper, 1:4)
})

test_that("a single fixed recipe reproduces idx_combine", {
  data <- indicator_fixture()
  out <- idx_sensitivity(data, c(a, b), id = unit, normalizations = "minmax",
                         weightings = "equal", aggregations = "arithmetic",
                         n_sims = 10, seed = 1, keep_draws = TRUE)
  index <- data |>
    idx_normalize(c(a, b), "minmax") |>
    idx_combine(c(a, b))
  expect_equal(out$summary$median_rank, rank(-index$index))

  expect_named(out, c("summary", "draws"))
  expect_equal(nrow(out$draws), 10 * 4)
  expect_named(out$draws, c("draw", "unit", "normalization", "weighting",
                            "aggregation", "index", "rank"))
})

test_that("idx_sensitivity validates its inputs", {
  data <- indicator_fixture()
  expect_error(idx_sensitivity(data, c(a, b), id = c(unit, a)), "exactly one")
  data_dup <- data
  data_dup$unit[2] <- "w"
  expect_error(idx_sensitivity(data_dup, c(a, b), id = unit), "unique")
  expect_error(idx_sensitivity(data, c(a, b), id = unit, weightings = "random"))
  expect_error(idx_sensitivity(data, c(a, b), id = unit, n_sims = 0), "n_sims")
})
