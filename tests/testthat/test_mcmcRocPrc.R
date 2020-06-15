
test_that("JAGS logit input works", {
  
  data("jags_logit")
  
  ## using mcmcRocPrc
  with_curves <- mcmcRocPrc(jags_logit,
                            yname = "Y",
                            xnames = c("X1", "X2"),
                            curves = TRUE,
                            fullsims = FALSE)
  
  ## testing
  value <- with_curves$prc_dat[[1]][156, 2]
  check_against <- c(0.658)
  expect_equal(value, check_against, tolerance = 1e-2)
  
  value_roc <- as.numeric(with_curves$area_under_roc)
  check_against_roc <- c(0.627)
  expect_equal(value_roc, check_against_roc, tolerance = 1e-2)
  
  value_prc <- as.numeric(with_curves$area_under_prc)
  check_against_prc <- c(0.621)
  expect_equal(value_prc, check_against_prc, tolerance = 1e-2)
  
})

test_that("JAGS probit input works", {
  
  data("jags_probit")
  
  expect_error(
    with_curves <- mcmcRocPrc(jags_probit,
                              yname = "Y",
                              xnames = c("X1", "X2"),
                              curves = TRUE,
                              fullsims = FALSE),
    NA
  )
  
})

test_that("Non-logit/probit JAGS does not work", {
  fake_jags <- structure(
    list(model = list(model = function() "incompatible model")),
    class = "rjags"
  )
  
  expect_error(mcmcRocPrc(fake_jags, 
                          yname = "Y",
                          xnames = c("X1", "X2")),
               "Could not identify model link function")
  
})


test_that("Simple model runs with mcmcRocPrc Full", {
  
  data("jags_logit")
  
  ## using mcmcRocPrc with full draws
  full_with_curves <- mcmcRocPrc(jags_logit,
                                 yname = "Y",
                                 xnames = c("X1", "X2"),
                                 curves = TRUE,
                                 fullsims = TRUE)
  
  ## testing
  value_area_under_roc <- as.numeric(full_with_curves$area_under_roc[986])
  check_against_full_roc <- c(0.626)
  expect_equal(value_area_under_roc, check_against_full_roc, tolerance = 1e-2)
  
  value_area_under_prc <- as.numeric(full_with_curves$area_under_prc[965])
  check_against_full_prc <- c(0.61994)
  expect_equal(value_area_under_prc, check_against_full_prc, tolerance = 1e-2)
  
})

# Now that it's established that basic creation works, set up examples of all
# four possible return value types that can be used for the rest of the tests
# below

with_curves <- mcmcRocPrc(jags_logit,
                          yname = "Y",
                          xnames = c("X1", "X2"),
                          curves = TRUE,
                          fullsims = FALSE)

no_curves <- mcmcRocPrc(jags_logit,
                        yname = "Y",
                        xnames = c("X1", "X2"),
                        curves = FALSE,
                        fullsims = FALSE)

full_no_curves <- mcmcRocPrc(jags_logit,
                             yname = "Y",
                             xnames = c("X1", "X2"),
                             curves = FALSE,
                             fullsims = TRUE)

full_with_curves <- mcmcRocPrc(jags_logit,
                               yname = "Y",
                               xnames = c("X1", "X2"),
                               curves = TRUE,
                               fullsims = TRUE)

test_that("mcmcRocPrc returns the same output structure, sans NULL elements", {
  
  expect_true(is.list(with_curves$prc_dat))
  expect_true(is.list(full_with_curves$prc_dat))
  
})


test_that("print method works", {
  
  expect_equal(
    capture_output(print(with_curves)), 
    "mcmcRocPrc object\ncurves: TRUE; fullsims: FALSE\nAUC-ROC: 0.627\nAUC-PR:  0.621"
  )
  expect_equal(
    capture_output(print(no_curves)), 
    "mcmcRocPrc object\ncurves: FALSE; fullsims: FALSE\nAUC-ROC: 0.627\nAUC-PR:  0.621"
  )
  expect_equal(
    capture_output(print(full_with_curves)), 
    "mcmcRocPrc object\ncurves: TRUE; fullsims: TRUE\nAUC-ROC: 0.624 [80%: 0.619 - 0.627]\nAUC-PR:  0.618 [80%: 0.613 - 0.620]"
  )
  expect_equal(
    capture_output(print(full_no_curves)), 
    "mcmcRocPrc object\ncurves: FALSE; fullsims: TRUE\nAUC-ROC: 0.624 [80%: 0.619 - 0.627]\nAUC-PR:  0.618 [80%: 0.613 - 0.620]"
  )
  
})

test_that("plot method gives informative errors", {
  
  expect_error(plot(no_curves), "to generate data for plots")
  expect_error(plot(with_curves, n = 0), "n must be")
  expect_error(plot(with_curves, alpha = 5), "alpha must be")
  
})

test_that("plot method works", {
  
  expect_error(plot(with_curves), NA)
  expect_error(plot(full_with_curves), NA)
  
  expect_error(plot(no_curves), "to generate data for plots")
  expect_error(plot(full_no_curves), "to generate data for plots")

})

# the plots from above will be sent to a Rplots.pdf file; clean that up
unlink("Rplots.pdf")

test_that("data frame conversion works with all 4 output sets", {
  
  # all 4 output types have AUC, so this should work across the board
  # (auc is the default what argument)
  expect_equal(nrow(as.data.frame(no_curves)), 1L)
  expect_equal(nrow(as.data.frame(with_curves)), 1L)
  expect_equal(nrow(as.data.frame(full_no_curves)), 2000L)
  expect_equal(nrow(as.data.frame(full_with_curves)), 2000L)
  
  # when called without curves, there will be no data for what = "roc"/"prc"
  expect_error(as.data.frame(no_curves, what = "roc"), "No curve data")
  expect_error(as.data.frame(no_curves, what = "prc"), "No curve data")
  expect_error(as.data.frame(full_no_curves, what = "roc"), "No curve data")
  expect_error(as.data.frame(full_no_curves, what = "roc"), "No curve data")
  
  # Otherwise, roc/prc data will either be average across sims, or a curve
  # for each sim. To ensure consistency in output, always return coordinate
  # data with an identifying "sim" column
  
  expect_error(out <- as.data.frame(with_curves, what = "roc"), NA)
  expect_s3_class(out, "data.frame")
  expect_equal(colnames(out), c("sim", "x", "y"))
  expect_equal(unique(out$sim), 1L)
  expect_equal(nrow(out), 501L)
  
  # fullsims
  expect_error(out <- as.data.frame(full_with_curves, what = "roc"), NA)
  expect_s3_class(out, "data.frame")
  expect_equal(colnames(out), c("sim", "x", "y"))
  expect_equal(length(unique(out$sim)), 2000L)
  expect_equal(nrow(out), 501L*2000L)
  
  
})

test_that("auc_roc and pr work", {
  
  expect_equal(auc_roc(c(0, 0, 1, 1), c(0, 0, 1, 1)), 1)
  expect_equal(auc_pr(c(0, 0, 1, 1), c(0, 0, 1, 1)), NaN)
  
})


