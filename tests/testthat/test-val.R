test_that("val_aggregate takes weighted means by group", {
  units <- data.frame(
    county = c("a", "a", "a", "b", "b"),
    score = c(10, 20, 40, 5, 15),
    pop = c(1, 1, 2, 3, 1)
  )
  # a: (10 + 20 + 2 * 40) / 4 = 27.5. b: (3 * 5 + 15) / 4 = 7.5.
  out <- val_aggregate(units, county, score, pop)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("county", "score", "pop", "n_units"))
  expect_equal(out$county, c("a", "b"))
  expect_equal(out$score, c(27.5, 7.5))
  expect_equal(out$pop, c(4, 4))
  expect_equal(out$n_units, c(3L, 2L))

  # Equal weights give the plain mean.
  units$one <- 1
  expect_equal(val_aggregate(units, county, score, one)$score,
               c(70 / 3, 10))
})

test_that("val_aggregate groups by several columns and drops geometry", {
  units <- data.frame(
    state = c("x", "x", "y", "y"),
    county = c("a", "b", "a", "a"),
    score = c(1, 2, 3, 5),
    pop = c(1, 1, 1, 3),
    lon = c(0, 1, 2, 3),
    lat = c(0, 0, 0, 0)
  )
  out <- val_aggregate(units, c(state, county), score, pop)
  expect_equal(nrow(out), 3)
  # y-a: (3 + 3 * 5) / 4 = 4.5.
  expect_equal(out$score[out$state == "y"], 4.5)

  pts <- sf::st_as_sf(units, coords = c("lon", "lat"))
  expect_false(inherits(val_aggregate(pts, state, score, pop), "sf"))
  expect_equal(val_aggregate(pts, state, score, pop)$score, c(1.5, 4.5))
})

test_that("val_aggregate handles missing scores and zero weights", {
  units <- data.frame(
    county = c("a", "a", "b", "c"),
    score = c(10, NA, 4, 7),
    pop = c(1, 3, 2, 0)
  )
  expect_message(kept <- val_aggregate(units, county, score, pop),
                 "zero total weight")
  expect_equal(kept$score, c(NA, 4, NA))
  expect_equal(kept$pop, c(4, 2, 0))

  dropped <- suppressMessages(val_aggregate(units, county, score, pop,
                                            na_rm = TRUE))
  expect_equal(dropped$score, c(10, 4, NA))
  expect_equal(dropped$pop, c(1, 2, 0))
  expect_equal(dropped$n_units, c(1L, 1L, 1L))
})

test_that("val_aggregate validates its inputs", {
  units <- data.frame(county = c("a", "b"), score = c(1, 2), pop = c(1, 2),
                      label = c("p", "q"))
  expect_error(val_aggregate(list(), county, score, pop), "data frame")
  expect_error(val_aggregate(units, county, c(score, pop), pop),
               "exactly one column")
  expect_error(val_aggregate(units, county, label, pop), "numeric")
  expect_error(val_aggregate(units, county, score, score), "different columns")
  expect_error(val_aggregate(units, county, county, pop), "different columns")
  units$pop[1] <- -1
  expect_error(val_aggregate(units, county, score, pop), "non-negative")
  expect_error(val_aggregate(units, county, score, missing_col))
  expect_error(val_aggregate(units, county, score, pop, na_rm = NA), "TRUE")
})

# Ten groups of unequal size with true intercepts drawn from N(0, 2^2).
group_effects_fixture <- function() {
  withr::with_seed(11, {
    sizes <- c(3, 5, 8, 10, 12, 15, 20, 25, 30, 40)
    g <- rep(sprintf("g%02d", 1:10), times = sizes)
    u <- stats::rnorm(10, sd = 2)[as.integer(factor(g))]
    x <- stats::rnorm(length(g))
    data.frame(g = g, x = x, y = 1 + 0.5 * x + u + stats::rnorm(length(g)))
  })
}

test_that("val_group_effects returns lme4's conditional modes and SEs", {
  skip_if_not_installed("lme4")
  data <- group_effects_fixture()
  fit <- lme4::lmer(y ~ x + (1 | g), data = data)
  out <- val_group_effects(fit, g)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("g", "estimate", "std_error", "lower", "upper", "n",
                      "excludes_zero"))
  expect_equal(nrow(out), 10)
  expect_true(all(diff(out$estimate) <= 0))

  re <- lme4::ranef(fit, condVar = TRUE)$g
  se <- sqrt(attr(re, "postVar")[1, 1, ])
  names(se) <- rownames(re)
  expect_equal(out$estimate, re[out$g, "(Intercept)"])
  expect_equal(out$std_error, unname(se[out$g]))

  z <- stats::qnorm(0.95)
  expect_equal(out$lower, out$estimate - z * out$std_error)
  expect_equal(out$upper, out$estimate + z * out$std_error)
  expect_equal(out$excludes_zero, out$lower > 0 | out$upper < 0)
  expect_equal(out$n[order(out$g)],
               c(3L, 5L, 8L, 10L, 12L, 15L, 20L, 25L, 30L, 40L))

  # Quoted names work, and a lower level gives narrower intervals.
  expect_equal(val_group_effects(fit, "g"), out)
  narrow <- val_group_effects(fit, g, level = 0.5)
  expect_true(all(narrow$upper - narrow$lower < out$upper - out$lower))
})

test_that("val_group_effects shrinks small groups more", {
  skip_if_not_installed("lme4")
  data <- group_effects_fixture()
  fit <- lme4::lmer(y ~ x + (1 | g), data = data)
  out <- val_group_effects(fit, g)
  # Raw group means of the residuals from the fixed part.
  resid <- data$y - stats::predict(fit, re.form = NA)
  raw <- tapply(resid, data$g, mean)[out$g]
  ratio <- out$estimate / raw
  expect_true(all(ratio > 0 & ratio < 1))
  expect_lt(ratio[out$n == 3], ratio[out$n == 40])
})

test_that("val_group_effects works on glmer fits", {
  skip_if_not_installed("lme4")
  data <- withr::with_seed(5, {
    g <- rep(sprintf("c%02d", 1:12), each = 40)
    u <- stats::rnorm(12, sd = 0.8)[as.integer(factor(g))]
    x <- stats::rnorm(length(g))
    data.frame(g = g, x = x,
               y = stats::rbinom(length(g), 1, stats::plogis(-0.5 + x + u)))
  })
  fit <- lme4::glmer(y ~ x + (1 | g), data = data, family = stats::binomial)
  out <- val_group_effects(fit, g)
  expect_equal(nrow(out), 12)
  expect_equal(sum(out$n), 480L)
  expect_true(all(out$std_error > 0))
})

test_that("val_group_effects validates its inputs", {
  skip_if_not_installed("lme4")
  data <- group_effects_fixture()
  fit <- lme4::lmer(y ~ x + (1 | g), data = data)
  expect_error(val_group_effects(stats::lm(y ~ x, data), g), "merMod")
  expect_error(val_group_effects(fit, county), "not a grouping factor")
  expect_error(val_group_effects(fit), "required")
  expect_error(val_group_effects(fit, g, level = 1.5), "between 0 and 1")
  slope_only <- suppressMessages(suppressWarnings(
    lme4::lmer(y ~ x + (0 + x | g), data = data)
  ))
  expect_error(val_group_effects(slope_only, g), "no random intercept")
})
