# Build `ca_place_concentration`: block-based measures of Hispanic or Latino
# residential concentration for every California place (incorporated places
# and census designated places), for the 2010 and 2020 censuses and for the
# total and voting-age populations.
#
# Needs a Census API key (see tidycensus::census_api_key()) and about 2 GB of
# tigris cache for the statewide block polygons. API calls run one at a time.
# Run from the package root with placeindex installed:
#   Rscript data-raw/ca_place_concentration.R
#
# Besides the dataset, the script writes data-raw/ca_place_validation.csv,
# which compares each place's block sums with the Census Bureau's own
# place-level counts.

library(placeindex)
library(dplyr)

options(tigris_use_cache = TRUE)

state <- "CA"
years <- c(2010, 2020)
n_districts <- 5

# Place-level counts for the validation and for place names. Codes as in
# seg_blocks(), verified with tidycensus::load_variables().
place_variables <- list(
  "2010" = c(total_total = "P004001", total_latino = "P004003",
             vap_total = "P011001", vap_latino = "P011002"),
  "2020" = c(total_total = "P2_001N", total_latino = "P2_002N",
             vap_total = "P4_001N", vap_latino = "P4_002N")
)

census_places <- function(year) {
  tidycensus::get_decennial(
    geography = "place",
    variables = place_variables[[as.character(year)]],
    year = year,
    sumfile = if (year == 2010) "sf1" else "pl",
    state = state,
    output = "wide"
  ) |>
    tidyr::pivot_longer(
      -c("GEOID", "NAME"),
      names_to = c("universe", ".value"),
      names_sep = "_"
    ) |>
    transmute(place_geoid = GEOID, place_name = NAME, year = as.integer(year),
              universe, census_total = total, census_latino = latino)
}

# Measures for one year and universe, from blocks inside places.
place_measures <- function(blocks) {
  in_place <- filter(blocks, !is.na(place_geoid))
  measures <- seg_measures(in_place, by = place_geoid, group = latino,
                           total = total)
  clusters <- seg_clusters(in_place, by = place_geoid, group = latino,
                           total = total, n_districts = n_districts)
  # seg_measures() keeps the order of first appearance and seg_clusters()
  # sorts, so join by place.
  out <- inner_join(measures, clusters, by = "place_geoid",
                    suffix = c("", ".clusters"), relationship = "one-to-one")
  stopifnot(nrow(out) == nrow(measures), nrow(out) == nrow(clusters),
            all(out$pop_total == out$pop_total.clusters))
  select(out, -"pop_total.clusters") |>
    mutate(year = blocks$year[1], universe = blocks$universe[1],
           .after = "place_geoid")
}

build_year <- function(year) {
  message("Census ", year, ": block counts and polygons")
  total <- seg_blocks(state, year, universe = "total")
  # Voting-age counts reuse the same polygons instead of reading them again.
  vap_counts <- seg_blocks(state, year, universe = "vap", geometry = FALSE)
  vap <- total |>
    select("block_geoid", "geometry") |>
    inner_join(vap_counts, by = "block_geoid", relationship = "one-to-one") |>
    relocate("geometry", .after = last_col())
  stopifnot(nrow(vap) == nrow(total), nrow(vap) == nrow(vap_counts))
  message("Blocks: ", nrow(total), "; in a place: ",
          sum(!is.na(total$place_geoid)))

  message("Census ", year, ": measures for the total population")
  out_total <- place_measures(total)
  message("Census ", year, ": measures for the voting-age population")
  out_vap <- place_measures(vap)
  bind_rows(out_total, out_vap)
}

timing <- system.time(results <- lapply(years, build_year))
message("Block run took ", round(timing[["elapsed"]] / 60, 1), " minutes")
measures <- bind_rows(results)

census <- bind_rows(lapply(years, census_places))

# Validation: block sums against the Census Bureau's place counts.
validation <- full_join(
  census,
  select(measures, "place_geoid", "year", "universe", "pop_total", "pop_group"),
  by = c("place_geoid", "year", "universe"),
  relationship = "one-to-one"
) |>
  mutate(
    diff_total = pop_total - census_total,
    diff_latino = pop_group - census_latino,
    match = !is.na(diff_total) & !is.na(diff_latino) &
      diff_total == 0 & diff_latino == 0
  )
readr::write_csv(validation, "data-raw/ca_place_validation.csv")
print(count(validation, year, universe, match))
print(filter(validation, !match), n = Inf, width = Inf)

ca_place_concentration <- measures |>
  left_join(distinct(census, place_geoid, year, place_name),
            by = c("place_geoid", "year"), relationship = "many-to-one") |>
  relocate("place_name", .after = "place_geoid") |>
  arrange(year, universe, place_geoid)
message("Places without a Census name: ",
        sum(is.na(ca_place_concentration$place_name)))

usethis::use_data(ca_place_concentration, overwrite = TRUE)
