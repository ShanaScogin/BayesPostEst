
test_that("Simple model runs with mcmcFD", {
  
  data("jags_logit")
  fit <- jags_logit
  
  data("sim_data")
  
  ## running function with logit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  object <- mcmcFD(modelmatrix = xmat,
                   mcmcout = mcmc_mat)
  
  value <- object[1, 2]
  check_against <- c(0.048)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
})

test_that("Simple probit model runs with mcmcFD", {
  
  data("jags_probit")
  fit <- jags_probit
  
  data("sim_data")
  
  ## running function with probit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  object <- mcmcFD(modelmatrix = xmat,
                   mcmcout = mcmc_mat,
                   link = "probit") ## check to see if these are correct
  
  value <- object[1, 2]
  check_against <- c(0.050)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  ## checking FD plot
  full <- mcmcFD(modelmatrix = xmat,
                 mcmcout = mcmc_mat,
                 fullsims = TRUE) # first running mcmcFD with full output
  expect_silent(mcmcFDplot(full))
  
})


test_that("ROPE version works", {
  
  data("jags_probit")
  fit <- jags_probit
  
  data("sim_data")
  
  ## running function with probit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  object <- mcmcFD(modelmatrix = xmat,
                   mcmcout = mcmc_mat,
                   link = "probit") ## check to see if these are correct
  
  value <- object[1, 2]
  check_against <- c(0.050)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  ## checking FD plot
  full <- mcmcFD(modelmatrix = xmat,
                 mcmcout = mcmc_mat,
                 fullsims = TRUE) # first running mcmcFD with full output
  expect_silent(mcmcFDplot(full, ROPE = c(0.1, 0.15)))
  
})
