# Knit the vignettes whose code needs the network (the rOpenSci .Rmd.orig
# pattern). R CMD check then builds the knitted .Rmd without any downloads.
# Needs a Census API key, and validation-mlm needs the CES 2024 Common
# Content CSV, its path set in CES_2024_PATH. Run from the package root
# with placeindex installed:
#   Rscript vignettes/precompute.R                  # all three vignettes
#   Rscript vignettes/precompute.R library-access   # one vignette
args <- commandArgs(trailingOnly = TRUE)
vignettes <- if (length(args) > 0) args else
  c("cvra-concentration", "library-access", "validation-mlm")
withr::with_dir("vignettes", {
  for (v in vignettes) knitr::knit(paste0(v, ".Rmd.orig"), paste0(v, ".Rmd"))
})
