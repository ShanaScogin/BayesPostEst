
test_that("Simple model runs with mcmcRocPrc", {
  
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

