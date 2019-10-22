test_that("Simple model runs with mcmcCoefPlot", {
  
  data("jags_logit")
  fit <- jags_logit
  
  expect_silent(mcmcCoefPlot(fit))

  expect_silent(mcmcCoefPlot(fit, pars = 'b'))
  
  expect_silent(mcmcCoefPlot(fit, pars = 'b', ci = .9, hpdi = T))
  
  ## test
  med_df <- mcmcCoefPlot(fit, pars = 'b', pointest = 'median', plot = F)
  value <- med_df[2, 1]
  check_against <- 0.5273031
  expect_equal(value, check_against, tolerance = 1e-4)

})

test_that("Simple model runs with mcmcCoefPlot with arguments", {
  
  data("jags_logit")
  fit <- jags_logit
  
  expect_silent(mcmcCoefPlot(fit))
  
  expect_silent(mcmcCoefPlot(fit, pars = 'b'))
  
  expect_silent(mcmcCoefPlot(fit, pars = 'b', ci = .9, hpdi = T))
  
  ## test
  med_df <- mcmcCoefPlot(fit, pars = 'b', pointest = 'median', plot = F)
  value <- med_df[2, 1]
  check_against <- 0.5273031
  expect_equal(value, check_against, tolerance = 1e-4)
  
})

test_that("Simple model runs with mcmcCoefPlot with sorting", {
  
  data("jags_logit")
  fit <- jags_logit
  
  ## test
  med_df <- mcmcCoefPlot(fit, pars = 'b', sort = T, plot = F)
  value <- med_df[3, 1]
  check_against <- 0.6335488
  expect_equal(value, check_against, tolerance = 1e-4)
  
})
