test_that("seg_measures matches hand-computed values", {
  out <- seg_measures(measures_fixture(), by = place, group = group, total = total)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$place, c("A", "B", "C"))
  expect_equal(out$n_blocks, c(5, 2, 2))
  expect_equal(out$pop_total, c(200, 40, 15))
  expect_equal(out$pop_group, c(80, 20, 0))
  expect_equal(out$group_share, c(0.4, 0.5, 0))
  expect_equal(out$dissimilarity, c(0.625, 0, NA))
  expect_equal(out$isolation, c(0.65, 0.5, NA))
  expect_equal(out$exposure, c(0.35, 0.5, NA))
  expect_equal(out$majority_share, c(0.875, 0, NA))
})

test_that("isolation and exposure sum to one", {
  out <- seg_measures(cluster_fixture(), by = place, group = group, total = total)
  expect_equal(out$isolation + out$exposure, c(1, 1))
})

test_that("seg_measures ignores geometry and accepts several by columns", {
  blocks <- cluster_fixture()
  from_sf <- seg_measures(blocks, place, group, total)
  from_df <- seg_measures(sf::st_drop_geometry(blocks), place, group, total)
  expect_equal(from_sf, from_df)

  blocks$year <- 2020L
  by_two <- seg_measures(blocks, c(place, year), group, total)
  expect_named(by_two)
  expect_equal(names(by_two)[1:2], c("place", "year"))
  expect_equal(by_two$dissimilarity, from_sf$dissimilarity)
})

test_that("seg_measures rejects invalid counts", {
  blocks <- measures_fixture()
  blocks$group[1] <- 60
  expect_error(seg_measures(blocks, place, group, total), "above")

  blocks <- measures_fixture()
  blocks$group <- as.character(blocks$group)
  expect_error(seg_measures(blocks, place, group, total), "numeric")

  expect_error(seg_measures(list(), place, group, total), "data frame")
})
