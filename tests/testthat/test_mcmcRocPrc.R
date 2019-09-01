test_that("Simple model runs with mcmcRocPrc", {
  
  data("jags_logit")
  fit <- jags_logit
  
  data("sim_data")
  
  ## processing the data
  mm <- model.matrix(Y ~ X1 + X2, data = sim_data)
  xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = sim_data))
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]
  
  ## using mcmcRocPrc
  fit_sum <- mcmcRocPrc(modelmatrix = mm,
                        modelframe = xframe,
                        mcmcout = mcmc_mat,
                        curves = TRUE,
                        fullsims = FALSE)
  
  ## testing
  value <- fit_sum$prc_dat[156, 2]
  check_against <- c(0.658)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  value_roc <- fit_sum$area_under_roc
  check_against_roc <- c(0.627)
  expect_equal(round(as.numeric(value_roc), 2), round(check_against_roc, 2))
  
  value_prc <- fit_sum$area_under_prc
  check_against_prc <- c(0.621)
  expect_equal(round(as.numeric(value_prc), 2), round(check_against_prc, 2))
  
})

test_that("Simple model runs with mcmcRocPrc Full", {
  skip_on_cran()
  ## this test is longer than CRAN allows
  
  data("jags_logit")
  fit <- jags_logit
  
  data("sim_data")

  ## processing the data
  mm <- model.matrix(Y ~ X1 + X2, data = sim_data)
  xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = sim_data))
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]

  ## using mcmcRocPrc with full draws
  fit_full <- mcmcRocPrc(modelmatrix = mm,
                         modelframe = xframe,
                         mcmcout = mcmc_mat,
                         curves = TRUE,
                         fullsims = TRUE)
  
  ## testing
  value_area_under_roc <- unlist(lapply(fit_full, '[[', 1))[986]
  check_against_full_roc <- c(0.626)
  expect_equal(round(as.numeric(value_area_under_roc), 2), 
               round(check_against_full_roc, 2))
  
  
  
  value_area_under_prc <- unlist(lapply(fit_full, '[[', 2))[965]
  check_against_full_prc <- c(0.61994)
  expect_equal(round(as.numeric(value_area_under_prc), 2), 
               round(check_against_full_prc, 2))
})
