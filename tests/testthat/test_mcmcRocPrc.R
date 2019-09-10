test_that("Simple model runs with mcmcRocPrc", {
  
  data("jags_logit")
  
  ## using mcmcRocPrc
  fit_sum <- mcmcRocPrc(jags_logit,
                        yname = "Y",
                        xnames = c("X1", "X2"),
                        curves = TRUE,
                        fullsims = FALSE)
  
  ## testing
  value <- fit_sum$prc_dat[156, 2]
  check_against <- c(0.658)
  expect_equal(value, check_against, tolerance = 1e-2)
  
  value_roc <- as.numeric(fit_sum$area_under_roc)
  check_against_roc <- c(0.627)
  expect_equal(value_roc, check_against_roc, tolerance = 1e-2)
  
  value_prc <- as.numeric(fit_sum$area_under_prc)
  check_against_prc <- c(0.621)
  expect_equal(value_prc, check_against_prc, tolerance = 1e-2)
  
})

test_that("Simple model runs with mcmcRocPrc Full", {
  
  data("jags_logit")

  ## using mcmcRocPrc with full draws
  fit_full <- mcmcRocPrc(jags_logit,
                         yname = "Y",
                         xnames = c("X1", "X2"),
                         curves = TRUE,
                         fullsims = TRUE)
  
  ## testing
  value_area_under_roc <- as.numeric(fit_full$area_under_roc[986])
  check_against_full_roc <- c(0.626)
  expect_equal(value_area_under_roc, check_against_full_roc, tolerance = 1e-2)
  
  value_area_under_prc <- as.numeric(fit_full$area_under_prc[965])
  check_against_full_prc <- c(0.61994)
  expect_equal(value_area_under_prc, check_against_full_prc, tolerance = 1e-2)
})
