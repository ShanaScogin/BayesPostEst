# Only remove the testdata folder if it exists
testdata_dir <- testthat::test_path("../testdata")
if (dir.exists(testdata_dir)) {
  unlink(file.path(testdata_dir, "*.rds"))
}
