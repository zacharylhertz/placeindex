#' Census blocks with place assignments and Hispanic or Latino counts
#'
#' Downloads decennial census block counts of the total and Hispanic or
#' Latino population for a state, assigns each block to its place with the
#' Census Bureau's Block Assignment File, and optionally attaches block
#' polygons.
#'
#' @param state State postal abbreviation, name, or two-digit FIPS code, such
#'   as `"CA"`.
#' @param year Decennial census year, 2010 or 2020.
#' @param universe `"total"` for the whole population or `"vap"` for the
#'   voting-age population (18 and over). For voting rights work use
#'   `"vap"`: courts look at citizen voting-age population (CVAP), which is
#'   not published for blocks, so VAP is the closest block-level count.
#' @param county Optional county names or three-digit FIPS codes to limit the
#'   download. `NULL` (the default) returns every county in the state, as
#'   listed in the Block Assignment File for that census year.
#' @param geometry If `TRUE` (the default), return an sf object with
#'   TIGER/Line block polygons.
#'
#' @details
#' Counts come from [tidycensus::get_decennial()], one county at a time, so a
#' Census API key is needed (see [tidycensus::census_api_key()]). The 2010
#' counts are from Summary File 1 and the 2020 counts from the PL 94-171
#' redistricting file:
#'
#' | Year | Universe | `total`   | `latino`  |
#' |------|----------|-----------|-----------|
#' | 2010 | total    | `P004001` | `P004003` |
#' | 2010 | vap      | `P011001` | `P011002` |
#' | 2020 | total    | `P2_001N` | `P2_002N` |
#' | 2020 | vap      | `P4_001N` | `P4_002N` |
#'
#' Places are incorporated places and census designated places. Each block is
#' assigned whole, using the `INCPLACE_CDP` table of the Block Assignment File
#' for the same census, rather than by overlaying geometries. Blocks outside
#' every place get `NA` in `place_geoid`.
#'
#' The Census Bureau added noise to 2020 block counts to protect
#' confidentiality, so they are more reliable summed over many blocks than
#' read one block at a time.
#'
#' Polygons come from [tigris::blocks()]. Set `options(tigris_use_cache =
#' TRUE)` to keep them between sessions. The 2020 block shapefile is
#' published only for whole states (382 MB zipped for California), so
#' `county` shortens the polygon download only for 2010.
#'
#' @return An sf object (a tibble when `geometry = FALSE`) with one row per
#'   block and columns `block_geoid`, `place_geoid` (state plus place FIPS
#'   code), `year`, `universe`, `total`, and `latino`.
#' @seealso [seg_measures()] and [seg_clusters()], which take this output.
#' @export
#' @examplesIf interactive()
#' options(tigris_use_cache = TRUE)
#' san_benito <- seg_blocks("CA", 2020, county = "San Benito")
#' seg_measures(san_benito, by = place_geoid, group = latino, total = total)
seg_blocks <- function(state, year, universe = c("total", "vap"),
                       county = NULL, geometry = TRUE) {
  state <- lookup_state(state)
  check_number(year, \(x) x %in% c(2010, 2020), "2010 or 2020")
  universe <- rlang::arg_match(universe)
  check_flag(geometry)

  places <- read_place_assignments(state, year)
  counties <- county %||% block_counties(places$block_geoid)
  variables <- block_variables(year, universe)
  counts <- lapply(counties, \(one_county) {
    tidycensus::get_decennial(
      geography = "block",
      variables = variables,
      year = year,
      sumfile = if (year == 2010) "sf1" else "pl",
      state = state$fips,
      county = one_county,
      output = "wide"
    )
  })

  blocks <- dplyr::bind_rows(counts) |>
    dplyr::select(block_geoid = "GEOID", "total", "latino") |>
    dplyr::left_join(places, by = "block_geoid", relationship = "one-to-one") |>
    dplyr::mutate(year = as.integer(year), universe = universe) |>
    dplyr::relocate("place_geoid", "year", "universe", .after = "block_geoid")

  if (!geometry) {
    return(blocks)
  }

  shapes <- tigris::blocks(
    state = state$fips,
    county = county,
    year = year,
    progress_bar = FALSE
  ) |>
    dplyr::select(block_geoid = dplyr::all_of(paste0("GEOID", year %% 100)))

  out <- dplyr::inner_join(shapes, blocks, by = "block_geoid",
                           relationship = "one-to-one")
  n_missing <- nrow(blocks) - nrow(out)
  if (n_missing > 0) {
    cli::cli_warn("{n_missing} block{?s} had counts but no polygon and {?was/were} dropped.")
  }
  dplyr::relocate(out, "geometry", .after = dplyr::last_col())
}

# Variable codes, verified with tidycensus::load_variables() on 2026-11-10.
block_variables <- function(year, universe) {
  codes <- list(
    "2010" = list(
      total = c(total = "P004001", latino = "P004003"),
      vap = c(total = "P011001", latino = "P011002")
    ),
    "2020" = list(
      total = c(total = "P2_001N", latino = "P2_002N"),
      vap = c(total = "P4_001N", latino = "P4_002N")
    )
  )
  codes[[as.character(year)]][[universe]]
}

lookup_state <- function(state, call = rlang::caller_env()) {
  states <- unique(tidycensus::fips_codes[c("state", "state_code", "state_name")])
  key <- toupper(as.character(state))
  match <- rlang::is_scalar_atomic(state) &&
    key %in% c(states$state, states$state_code, toupper(states$state_name))
  if (!match) {
    cli::cli_abort(
      "{.arg state} must be one state's abbreviation, name, or FIPS code.",
      call = call
    )
  }
  row <- states[key == states$state | key == states$state_code |
                  key == toupper(states$state_name), ]
  list(fips = row$state_code, abbr = row$state)
}

# County codes of the census year, read from the block IDs in the Block
# Assignment File. tidycensus::fips_codes is not used here because it mixes
# vintages: it also lists county codes that have no blocks in a given census,
# such as Connecticut's planning regions (adopted after 2020) and retired
# Alaska, South Dakota and Virginia codes, so looping over it would request
# blocks for counties that do not exist.
block_counties <- function(block_geoid) {
  unique(substr(block_geoid, 3, 5))
}

baf_url <- function(state, year) {
  folder <- if (year == 2010) "baf" else "baf2020"
  sprintf(
    "https://www2.census.gov/geo/docs/maps-data/data/%s/BlockAssign_ST%s_%s.zip",
    folder, state$fips, state$abbr
  )
}

# Download the state's Block Assignment File once per session.
read_place_assignments <- function(state, year) {
  zip <- file.path(tempdir(), sprintf("baf%s_%s.zip", year, state$abbr))
  if (!file.exists(zip)) {
    withr::local_options(timeout = max(600, getOption("timeout")))
    utils::download.file(baf_url(state, year), zip, mode = "wb", quiet = TRUE)
  }
  parse_place_assignments(zip, state, year)
}

# The 2010 file is comma-delimited with CRLF line endings; the 2020 file is
# pipe-delimited. Both leave PLACEFP blank for blocks outside every place.
parse_place_assignments <- function(zip, state, year) {
  file <- sprintf("BlockAssign_ST%s_%s_INCPLACE_CDP.txt", state$fips, state$abbr)
  baf <- utils::read.table(
    unz(zip, file),
    header = TRUE,
    sep = if (year == 2010) "," else "|",
    colClasses = "character",
    na.strings = "",
    quote = ""
  )
  dplyr::tibble(
    block_geoid = baf$BLOCKID,
    place_geoid = ifelse(is.na(baf$PLACEFP), NA_character_,
                         paste0(state$fips, baf$PLACEFP))
  )
}
