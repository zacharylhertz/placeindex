# Hand-checked E2SFCA example: 3 origins, 2 sites, catchment 10.
#
#   Origin populations: a = 100, b = 200, c = 300.
#   Site capacities:    x = 10,  y = 30.
#   Costs: a-x 0, a-y 20 (out of reach), b-x 8, b-y 4,
#          c-x 15 (out of reach), c-y 0.
#
# Step decay, bands (0, 5] -> 1 and (5, 10] -> 0.5:
#   Step 1: x has a (100 * 1) + b (200 * 0.5) = 200 in reach, R_x = 10 / 200 = 0.05.
#           y has b (200 * 1) + c (300 * 1)   = 500 in reach, R_y = 30 / 500 = 0.06.
#   Step 2: a = 0.05 * 1              = 0.05
#           b = 0.05 * 0.5 + 0.06 * 1 = 0.085
#           c = 0.06 * 1              = 0.06
#   Check: 100 * 0.05 + 200 * 0.085 + 300 * 0.06 = 5 + 17 + 18 = 40 = 10 + 30,
#   so all supply is shared out.
#
# Gaussian decay, g(d) = (exp(-(d / 10)^2 / 2) - exp(-1/2)) / (1 - exp(-1/2)):
#   g(0) = 1, g(4) = (0.92312 - 0.60653) / 0.39347 = 0.80460,
#   g(8) = (0.72615 - 0.60653) / 0.39347 = 0.30401.
#   Step 1: R_x = 10 / (100 * 1 + 200 * 0.30401) = 10 / 160.80 = 0.062188.
#           R_y = 30 / (200 * 0.80460 + 300 * 1) = 30 / 460.92 = 0.065087.
#   Step 2: a = 0.062188
#           b = 0.062188 * 0.30401 + 0.065087 * 0.80460 = 0.018906 + 0.052369 = 0.071275
#           c = 0.065087
access_fixture <- function() {
  list(
    demand = data.frame(id = c("a", "b", "c"), pop = c(100, 200, 300)),
    supply = data.frame(site = c("x", "y"), cap = c(10, 30)),
    cost = data.frame(
      from = c("a", "a", "b", "b", "c", "c"),
      to = c("x", "y", "x", "y", "x", "y"),
      cost = c(0, 20, 8, 4, 15, 0)
    )
  )
}

run_fixture <- function(f, ...) {
  acc_e2sfca(f$demand, f$supply, f$cost, id, pop, site, cap,
             catchment = 10, ...)
}

test_that("acc_e2sfca matches the hand calculation with step decay", {
  f <- access_fixture()
  out <- run_fixture(f, decay = "step",
                     bands = data.frame(upper = c(5, 10), weight = c(1, 0.5)))
  expect_equal(out$id, c("a", "b", "c"))
  expect_equal(out$access, c(0.05, 0.085, 0.06))
  expect_equal(out$n_sites, c(1L, 2L, 1L))
  expect_equal(sum(f$demand$pop * out$access), 40)
})

test_that("acc_e2sfca matches the hand calculation with Gaussian decay", {
  f <- access_fixture()
  out <- run_fixture(f)
  expect_equal(out$access, c(0.062188, 0.071275, 0.065087), tolerance = 1e-4)
  expect_equal(sum(f$demand$pop * out$access), 40)
})

test_that("acc_e2sfca default step bands follow Luo and Qi", {
  f <- access_fixture()
  # Bands (0, 10/3] -> 1, (10/3, 20/3] -> 0.68, (20/3, 10] -> 0.22.
  # b-x (8) gets 0.22 and b-y (4) gets 0.68.
  out <- run_fixture(f, decay = "step")
  r_x <- 10 / (100 + 200 * 0.22)
  r_y <- 30 / (200 * 0.68 + 300)
  expect_equal(out$access, c(r_x, r_x * 0.22 + r_y * 0.68, r_y))
})

test_that("acc_e2sfca handles unreachable origins and empty sites", {
  f <- access_fixture()
  f$demand <- rbind(f$demand, data.frame(id = "d", pop = 50))
  f$supply <- rbind(f$supply, data.frame(site = "z", cap = 99))
  f$cost <- rbind(f$cost, data.frame(from = "a", to = "z", cost = 30))
  out <- run_fixture(f, decay = "step",
                     bands = data.frame(upper = c(5, 10), weight = c(1, 0.5)))
  # Origin d has no pair and site z is out of everyone's reach.
  expect_equal(out$access, c(0.05, 0.085, 0.06, 0))
  expect_equal(out$n_sites, c(1L, 2L, 1L, 0L))

  # A site whose catchment holds only zero-population origins adds nothing.
  f2 <- access_fixture()
  f2$demand$pop[1] <- 0
  f2$cost$cost[3] <- 12 # b-x now out of reach, so x serves only a (pop 0)
  out2 <- run_fixture(f2, decay = "step",
                      bands = data.frame(upper = c(5, 10), weight = c(1, 0.5)))
  expect_equal(out2$access[1], 0)
  expect_true(all(is.finite(out2$access)))
})

test_that("acc_e2sfca validates its inputs", {
  f <- access_fixture()
  expect_error(run_fixture(f, decay = "step",
                           bands = data.frame(upper = c(5, 8), weight = c(1, 0.5))),
               "must equal")
  expect_error(run_fixture(f, bands = data.frame(upper = 10, weight = 1)),
               "only used")
  expect_error(run_fixture(f, decay = "step",
                           bands = data.frame(upper = c(5, 10), weight = c(1, 2))),
               "between 0 and 1")
  bad <- f
  bad$demand$pop[2] <- -1
  expect_error(run_fixture(bad), "non-negative")
  bad <- f
  bad$cost <- rbind(bad$cost, bad$cost[1, ])
  expect_error(run_fixture(bad), "repeats")
  bad <- f
  bad$cost$cost[1] <- NA
  expect_error(run_fixture(bad), "non-missing")
  expect_error(
    acc_e2sfca(f$demand, f$supply, f$cost, id, pop, site, cap, catchment = -1),
    "positive"
  )
  expect_error(
    acc_e2sfca(f$demand, f$supply, f$cost, population = pop, supply_id = site,
               capacity = cap, catchment = 10),
    "required"
  )
  bad <- f
  bad$cost$to[1] <- "nowhere"
  expect_message(run_fixture(bad), "Ignoring 1 row")
})

test_that("acc_e2sfca stops when no cost row matches the IDs", {
  f <- access_fixture()
  # IDs of a different format, such as GEOIDs that lost a prefix, would
  # otherwise give every origin zero access.
  f$cost$from <- paste0("0", f$cost$from)
  expect_error(run_fixture(f), "No row of `cost`")

  # A cost table with no rows means nothing is in reach: zero access.
  f <- access_fixture()
  f$cost <- f$cost[0, ]
  out <- run_fixture(f)
  expect_equal(out$access, c(0, 0, 0))
  expect_equal(out$n_sites, c(0L, 0L, 0L))
})

# Euclidean fixture in a projected CRS, built from 3-4-5 triangles.
#   Origins a (0, 0), b (3, 4), c (6, 8); sites x (0, 0), y (6, 0).
#   a-x 0, a-y 6, b-x 5, b-y sqrt(9 + 16) = 5, c-x 10, c-y sqrt(0 + 64) = 8.
points_fixture <- function(crs = 3310) {
  list(
    origins = sf::st_as_sf(
      data.frame(id = c("a", "b", "c"), x = c(0, 3, 6), y = c(0, 4, 8)),
      coords = c("x", "y"), crs = crs
    ),
    sites = sf::st_as_sf(
      data.frame(site = c("x", "y"), x = c(0, 6), y = c(0, 0)),
      coords = c("x", "y"), crs = crs
    )
  )
}

test_that("acc_cost_matrix computes straight-line distances", {
  p <- points_fixture()
  out <- acc_cost_matrix(p$origins, p$sites, id, site)
  expect_equal(out$from, c("a", "a", "b", "b", "c", "c"))
  expect_equal(out$to, c("x", "y", "x", "y", "x", "y"))
  expect_equal(out$cost, c(0, 6, 5, 5, 10, 8))

  capped <- acc_cost_matrix(p$origins, p$sites, id, site, max_cost = 6)
  expect_equal(capped$cost, c(0, 6, 5, 5))
  expect_equal(nrow(capped), 4)
})

test_that("acc_cost_matrix checks geometry and CRS for euclidean costs", {
  p <- points_fixture()
  expect_error(
    acc_cost_matrix(sf::st_transform(p$origins, 4326),
                    sf::st_transform(p$sites, 4326), id, site),
    "projected"
  )
  expect_error(
    acc_cost_matrix(p$origins, sf::st_transform(p$sites, 3857), id, site),
    "share a CRS"
  )
  polys <- sf::st_buffer(p$origins, 1)
  expect_error(acc_cost_matrix(polys, p$sites, id, site), "POINT")
  expect_error(
    acc_cost_matrix(p$origins, p$sites, id, site, cost = data.frame()),
    "only used"
  )
  dup <- p$origins
  dup$id[2] <- "a"
  expect_error(acc_cost_matrix(dup, p$sites, id, site), "unique")
})

test_that("acc_cost_matrix validates and filters a provided table", {
  p <- points_fixture()
  times <- data.frame(from = c("a", "b", "z", "c"), to = c("x", "y", "x", "y"),
                      cost = c(5, 12, 3, 30))
  expect_message(
    out <- acc_cost_matrix(p$origins, p$sites, id, site,
                           method = "provided", cost = times, max_cost = 20),
    "Dropped 1 row"
  )
  expect_equal(out$from, c("a", "b"))
  expect_equal(out$cost, c(5, 12))
  expect_named(out, c("from", "to", "cost"))

  expect_error(
    acc_cost_matrix(p$origins, p$sites, id, site, method = "provided"),
    "needs"
  )
  expect_error(
    acc_cost_matrix(p$origins, p$sites, id, site, method = "provided",
                    cost = data.frame(origin_id = "a", to = "x", cost = 1)),
    "missing"
  )
  expect_error(
    acc_cost_matrix(p$origins, p$sites, id, site, method = "provided",
                    cost = data.frame(from = "q", to = "x", cost = 1)),
    "No row"
  )
  expect_error(
    acc_cost_matrix(p$origins, p$sites, id, site, method = "provided",
                    cost = data.frame(from = "a", to = "x", cost = -1)),
    "non-negative"
  )
})

test_that("acc_centroids weights block points by population", {
  # t1: blocks at (0, 0) pop 1, (4, 0) pop 3, (0, 4) pop 0
  #     -> x = (0 + 12 + 0) / 4 = 3, y = 0.
  # t2: two empty blocks at (10, 10) and (12, 10) -> unweighted mean (11, 10).
  blocks <- sf::st_as_sf(
    data.frame(
      tract = c("t1", "t1", "t1", "t2", "t2"),
      pop = c(1, 3, 0, 0, 0),
      x = c(0, 4, 0, 10, 12),
      y = c(0, 0, 4, 10, 10)
    ),
    coords = c("x", "y"), crs = 3310
  )
  expect_message(out <- acc_centroids(blocks, tract, pop), "zero total")
  expect_s3_class(out, "sf")
  expect_equal(out$tract, c("t1", "t2"))
  expect_equal(out$pop, c(4, 0))
  expect_equal(out$n_blocks, c(3L, 2L))
  expect_equal(unname(sf::st_coordinates(out)), cbind(c(3, 11), c(0, 10)))
  expect_equal(sf::st_crs(out), sf::st_crs(blocks))

  # Polygons are reduced to a point inside each one.
  squares <- sf::st_buffer(blocks[1:2, ], 0.5, endCapStyle = "SQUARE")
  sq <- acc_centroids(squares, tract, pop)
  expect_equal(unname(sf::st_coordinates(sq)), cbind(3, 0), tolerance = 1e-6)

  bad <- blocks
  bad$pop[1] <- NA
  expect_error(acc_centroids(bad, tract, pop), "non-missing")
  expect_error(acc_centroids(sf::st_drop_geometry(blocks), tract, pop), "sf")
  expect_error(acc_centroids(blocks, c(tract, pop), pop), "exactly one")
})
