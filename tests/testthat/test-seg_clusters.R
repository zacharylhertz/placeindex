test_that("seg_clusters finds the built-in clusters without bridging", {
  out <- seg_clusters(cluster_fixture(), place, group, total,
                      n_districts = 5, bridge_empty = FALSE)
  # The block outside every place is dropped.
  expect_equal(out$place, c("A", "B"))

  a <- out[out$place == "A", ]
  expect_equal(a$pop_total, 2450)
  expect_equal(a$n_clusters, 4)
  expect_equal(a$largest_pop, 400)
  expect_equal(a$largest_group, 320)
  expect_equal(a$largest_share, 0.8)
  # Ideal district: 2450 / 5 = 490, which no cluster reaches.
  expect_equal(a$ideal_pop, 490)
  expect_equal(a$largest_ratio, 400 / 490)
  expect_equal(a$n_viable, 0)
})

test_that("empty blocks bridge clusters when asked", {
  out <- seg_clusters(cluster_fixture(), place, group, total,
                      n_districts = 5, bridge_empty = TRUE)
  a <- out[out$place == "A", ]
  # C and D merge through the empty cell: pop 500, group 380.
  expect_equal(a$n_clusters, 3)
  expect_equal(a$largest_pop, 500)
  expect_equal(a$largest_group, 380)
  expect_equal(a$largest_share, 0.76)
  expect_equal(a$largest_ratio, 500 / 490)
  expect_equal(a$n_viable, 1)
})

test_that("clusters stop at place boundaries", {
  out <- seg_clusters(cluster_fixture(), place, group, total, n_districts = 5)
  b <- out[out$place == "B", ]
  # B's majority block touches cluster D in place A but stays separate.
  expect_equal(b$pop_total, 200)
  expect_equal(b$n_clusters, 1)
  expect_equal(b$largest_pop, 100)
  expect_equal(b$largest_share, 1)
  expect_equal(b$ideal_pop, 40)
  expect_equal(b$largest_ratio, 2.5)
  expect_equal(b$n_viable, 1)
})

test_that("queen contiguity joins blocks that share a corner", {
  bridged <- seg_clusters(cluster_fixture(), place, group, total,
                          contiguity = "queen")
  # A + B (pop 450) and C + empty + D (pop 500).
  expect_equal(bridged$n_clusters[1], 2)
  expect_equal(bridged$largest_pop[1], 500)

  unbridged <- seg_clusters(cluster_fixture(), place, group, total,
                            contiguity = "queen", bridge_empty = FALSE)
  # A + B (pop 450), C (200), D (300).
  expect_equal(unbridged$n_clusters[1], 3)
  expect_equal(unbridged$largest_pop[1], 450)
  expect_equal(unbridged$largest_group[1], 360)
})

test_that("threshold sets which blocks qualify", {
  # Above 0.75, C's blocks (0.70) drop out; the empty cell joins D alone.
  out <- seg_clusters(cluster_fixture(), place, group, total, threshold = 0.75)
  expect_equal(out$n_clusters[1], 3)
  expect_equal(out$largest_pop[1], 400)

  # Above 0.95 nothing in A qualifies.
  none <- seg_clusters(cluster_fixture(), place, group, total, threshold = 0.95)
  expect_equal(none$n_clusters[1], 0)
  expect_equal(none$largest_pop[1], 0)
  expect_true(is.na(none$largest_share[1]))
})

test_that("n_districts can be a column and is optional", {
  blocks <- cluster_fixture()
  blocks$seats <- 5
  expect_equal(
    seg_clusters(blocks, place, group, total, n_districts = seats),
    seg_clusters(blocks, place, group, total, n_districts = 5)
  )

  out <- seg_clusters(blocks, place, group, total)
  expect_false(any(c("ideal_pop", "n_viable") %in% names(out)))

  blocks$seats[1] <- 7
  expect_error(seg_clusters(blocks, place, group, total, n_districts = seats),
               "constant")
  expect_error(seg_clusters(blocks, place, group, total, n_districts = 0),
               "positive whole")
})

test_that("seg_clusters validates its inputs", {
  blocks <- cluster_fixture()
  expect_error(seg_clusters(sf::st_drop_geometry(blocks), place, group, total),
               "sf object")
  expect_error(seg_clusters(blocks, place, group, total, threshold = 1),
               "threshold")
  expect_error(seg_clusters(blocks, place, group, total, bridge_empty = NA),
               "bridge_empty")
})

test_that("n_districts_floor sums whole ideal districts across clusters", {
  # Bridged, five districts: ideal 490. A's clusters (500, 400, 50) hold
  # 1 + 0 + 0. B's cluster of 100 against an ideal of 40 holds 2, but it is
  # one cluster, so n_viable is 1.
  five <- seg_clusters(cluster_fixture(), place, group, total, n_districts = 5)
  expect_equal(five$n_districts_floor, c(1, 2))
  expect_equal(five$n_viable, c(1, 1))

  # Ten districts: ideal 245. A's clusters hold 2 + 1 + 0 = 3, and two of
  # them reach the ideal.
  ten <- seg_clusters(cluster_fixture(), place, group, total, n_districts = 10)
  expect_equal(ten$n_districts_floor[1], 3)
  expect_equal(ten$n_viable[1], 2)

  # Without bridging, A's clusters (400, 300, 200, 50) hold 1 + 1 + 0 + 0.
  unbridged <- seg_clusters(cluster_fixture(), place, group, total,
                            n_districts = 10, bridge_empty = FALSE)
  expect_equal(unbridged$n_districts_floor[1], 2)
  expect_equal(unbridged$n_viable[1], 2)

  # A cluster of exactly one ideal district counts: with two districts B's
  # ideal is 100 and its cluster is 100.
  two <- seg_clusters(cluster_fixture(), place, group, total, n_districts = 2)
  expect_equal(two$n_districts_floor[2], 1)
  expect_equal(two$n_viable[2], 1)

  out <- seg_clusters(cluster_fixture(), place, group, total)
  expect_false("n_districts_floor" %in% names(out))
})

test_that("seg_cluster_blocks labels blocks by cluster size", {
  # Cells are numbered from the bottom left, row by row (see the fixture).
  bridged <- seg_cluster_blocks(cluster_fixture(), place, group, total)
  # The block outside every place is dropped.
  expect_equal(nrow(bridged), 27)
  expect_s3_class(bridged, "sf")
  expect_false(any(c(".group", ".total") %in% names(bridged)))
  expect_equal(
    bridged$cluster,
    c(2, 2, NA, NA, NA,      # row 1: cluster A
      2, 2, NA, NA, NA,      # row 2: cluster A
      NA, NA, 3, NA, NA,     # row 3: cluster B
      NA, NA, NA, NA, NA,    # row 4
      1, 1, 1, 1, 1,         # row 5: C, the empty cell, and D
      1, NA)                 # place B
  )

  unbridged <- seg_cluster_blocks(cluster_fixture(), place, group, total,
                                  bridge_empty = FALSE)
  # A (400) is 1, D (300) is 2, C (200) is 3, B (50) is 4; the empty cell
  # is in no cluster.
  expect_equal(unbridged$cluster[c(1, 13, 21, 23, 24)], c(1, 4, 3, NA, 2))

  # The labels agree with seg_clusters().
  summary <- seg_clusters(cluster_fixture(), place, group, total)
  in_a <- bridged$place == "A" & bridged$cluster %in% 1
  expect_equal(sum(bridged$total[in_a]), summary$largest_pop[1])

  expect_error(seg_cluster_blocks(sf::st_drop_geometry(cluster_fixture()),
                                  place, group, total), "sf object")
})
