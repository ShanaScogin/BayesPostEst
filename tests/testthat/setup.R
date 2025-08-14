# tests/testthat/setup.R

# first the packages
# formerly in helper-lib.R
library(R2jags)
library(runjags)

testdata_dir <- testthat::test_path("../testdata")
dir.create(testdata_dir, showWarnings = FALSE, recursive = TRUE)

# Make TESTDATA_DIR available to all scripts AND tests
assign("TESTDATA_DIR", testdata_dir, envir = .GlobalEnv)

# Source sim_data.R first
source(testthat::test_path("setup-data/sim_data.R"))

# Source the rest of the setup-data scripts
r_scripts <- list.files(
  testthat::test_path("setup-data"),
  pattern = "\\.R$",
  full.names = TRUE
)
r_scripts <- setdiff(r_scripts, testthat::test_path("setup-data/sim_data.R"))
lapply(r_scripts, source)
