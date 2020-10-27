data("jags_logit")
data("runjags_interactive")
data("mcmcpack_linear")
data("bugs_model")

test_that("Simple model runs with mcmcCoefPlot", {
  
  expect_silent(mcmcCoefPlot(jags_logit))

})

test_that("mcmcCoefPlot works with multiple object types", {
  
  ## runjags
  expect_silent(mcmcCoefPlot(runjags_interactive))
  
  ## mcmc.list
  expect_silent(mcmcCoefPlot(mcmcpack_linear))
  
  ## mcmc
  expect_silent(mcmcCoefPlot(coda::as.mcmc(jags_logit)[[1]]))
  
  ## bugs
  expect_silent(mcmcCoefPlot(bugs_model))
  
})

test_that("Simple model runs with mcmcCoefPlot with arguments", {
  
  expect_silent(mcmcCoefPlot(jags_logit))
  
  expect_silent(mcmcCoefPlot(jags_logit, pars = 'b', regex = T))
  
  expect_silent(mcmcCoefPlot(jags_logit, pars = 'b', ci = .9, hpdi = T, regex = T))
  
  ## test
  med_df <- mcmcCoefPlot(jags_logit, pars = 'b', pointest = 'median', plot = F, regex = T)
  value <- med_df[2, 1]
  check_against <- 0.5273031
  expect_equal(value, check_against, tolerance = 1e-4)
  
  expect_silent(mcmcCoefPlot(jags_logit, pars = c('b[1]', 'b[2]'), regex = FALSE))
  
})

test_that("Simple model runs with mcmcCoefPlot with sorting", {
  
  ## test
  med_df <- mcmcCoefPlot(jags_logit, pars = 'b', sort = T, plot = F, regex = T)
  value <- med_df[3, 1]
  check_against <- 0.6335488
  expect_equal(value, check_against, tolerance = 1e-4)
  
})

test_that("mcmcCoefPlot errors work", {
  
  expect_error(mcmcCoefPlot(jags_logit, pointest = 'man'))
  expect_error(mcmcCoefPlot(jags_logit, hpdi = 2))
  
})