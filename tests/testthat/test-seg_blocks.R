# These tests never touch the network: seg_blocks() validates its inputs
# before downloading anything.

test_that("seg_blocks validates inputs before downloading", {
  expect_error(seg_blocks("CA", 2015), "2010 or 2020")
  expect_error(seg_blocks("Narnia", 2020), "state")
  expect_error(seg_blocks("CA", 2020, universe = "adults"), "universe")
  expect_error(seg_blocks("CA", 2020, geometry = "yes"), "geometry")
})

test_that("states can be given by abbreviation, name, or FIPS code", {
  expected <- list(fips = "06", abbr = "CA")
  expect_equal(lookup_state("CA"), expected)
  expect_equal(lookup_state("california"), expected)
  expect_equal(lookup_state("06"), expected)
})

test_that("variable codes follow year and universe", {
  expect_equal(block_variables(2010, "total"),
               c(total = "P004001", latino = "P004003"))
  expect_equal(block_variables(2020, "vap"),
               c(total = "P4_001N", latino = "P4_002N"))
})

test_that("counties come from the block IDs of the census year", {
  # Characters 3 to 5 of a block GEOID are the county. Each county appears
  # once, in the order of the file. tidycensus::fips_codes would also list
  # Connecticut's planning regions (110 to 190), which have no 2020 blocks.
  ids <- c("090010101001000", "090010101001001", "090150101001000",
           "090030101001000")
  expect_equal(block_counties(ids), c("001", "015", "003"))
})

test_that("seg_blocks requests exactly the counties in the assignment file", {
  # Both downloads are replaced, so the test stays offline.
  local_mocked_bindings(
    read_place_assignments = function(state, year) {
      dplyr::tibble(block_geoid = c("090010101001000", "090150101001000"),
                    place_geoid = c("0912345", NA))
    }
  )
  requested <- character()
  local_mocked_bindings(
    get_decennial = function(..., county) {
      requested <<- c(requested, county)
      dplyr::tibble(GEOID = paste0("09", county, "0101001000"), NAME = "x",
                    total = 10, latino = 4)
    },
    .package = "tidycensus"
  )
  out <- seg_blocks("CT", 2020, geometry = FALSE)
  expect_equal(requested, c("001", "015"))
  expect_equal(out$place_geoid, c("0912345", NA))
  expect_equal(out$latino, c(4, 4))
})

test_that("Block Assignment Files of both years parse the same way", {
  skip_if(!nzchar(Sys.which("zip")), "no zip program")
  state <- list(fips = "06", abbr = "CA")
  dir <- withr::local_tempdir()
  file <- file.path(dir, "BlockAssign_ST06_CA_INCPLACE_CDP.txt")
  expected <- dplyr::tibble(
    block_geoid = c("060690001001000", "060690001001001"),
    place_geoid = c("0634120", NA)
  )

  # 2010: comma-delimited, CRLF line endings, blank PLACEFP outside places.
  writeBin(charToRaw("BLOCKID,PLACEFP\r\n060690001001000,34120\r\n060690001001001,\r\n"), file)
  zip_2010 <- file.path(dir, "baf2010.zip")
  utils::zip(zip_2010, file, flags = "-j -q")
  expect_equal(parse_place_assignments(zip_2010, state, 2010), expected)

  # 2020: pipe-delimited.
  writeLines(c("BLOCKID|PLACEFP", "060690001001000|34120", "060690001001001|"), file)
  zip_2020 <- file.path(dir, "baf2020.zip")
  utils::zip(zip_2020, file, flags = "-j -q")
  expect_equal(parse_place_assignments(zip_2020, state, 2020), expected)
})
