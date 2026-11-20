#' Latino residential concentration in California places, 2010 and 2020
#'
#' Block-based measures of how concentrated the Hispanic or Latino
#' population is within every California place (incorporated places and
#' census designated places), for the 2010 and 2020 censuses and for the
#' total and voting-age populations. Built with [seg_blocks()],
#' [seg_measures()] and [seg_clusters()] by `data-raw/ca_place_concentration.R`
#' in the package source.
#'
#' For voting rights work use `universe == "vap"`. Courts look at citizen
#' voting-age population, which is not published for blocks, so the
#' voting-age population is the closest block-level count.
#'
#' The district columns assume a five-district council, with
#' `seg_clusters()` defaults: majority means above 50 percent, rook
#' contiguity, and empty blocks may bridge clusters. For another council
#' size `k`, `largest_pop / (pop_total / k)` gives the largest cluster in
#' ideal districts. `n_viable` and `n_districts_floor` need every cluster's
#' population, so recompute them with [seg_clusters()].
#'
#' @format A tibble with one row per place, year and universe:
#' \describe{
#'   \item{place_geoid}{Seven-digit place code: state FIPS code `06` and the
#'     place FIPS code.}
#'   \item{place_name}{Census place name, such as "Palmdale city, California".}
#'   \item{year}{Census year, 2010 or 2020.}
#'   \item{universe}{`"total"` for the whole population or `"vap"` for the
#'     voting-age population (18 and over).}
#'   \item{n_blocks}{Number of census blocks in the place.}
#'   \item{pop_total, pop_group}{Population and Hispanic or Latino
#'     population, summed over blocks.}
#'   \item{group_share}{Hispanic or Latino share of the population.}
#'   \item{dissimilarity, isolation, exposure}{Block-level segregation
#'     measures of Latinos against everyone else; see [seg_measures()].}
#'   \item{majority_share}{Share of Latinos living in majority-Latino blocks.}
#'   \item{n_clusters}{Number of contiguous clusters of majority-Latino
#'     blocks.}
#'   \item{largest_pop, largest_group, largest_share}{Population, Latino
#'     population and Latino share of the largest cluster.}
#'   \item{n_districts}{Council size assumed: 5.}
#'   \item{ideal_pop}{`pop_total / n_districts`.}
#'   \item{largest_ratio}{`largest_pop / ideal_pop`; `NaN` for the few places
#'     with no population.}
#'   \item{n_viable}{Clusters with at least `ideal_pop` people.}
#'   \item{n_districts_floor}{Sum over clusters of whole ideal districts, a
#'     heuristic count of majority-Latino districts; see [seg_clusters()].}
#' }
#' @source U.S. Census Bureau: 2010 Census Summary File 1 (tables P4 and
#'   P11) and 2020 Census Redistricting Data (P.L. 94-171, tables P2 and P4),
#'   through the Census API; Block Assignment Files for places; TIGER/Line
#'   block shapefiles. These are U.S. government works in the public domain.
"ca_place_concentration"
