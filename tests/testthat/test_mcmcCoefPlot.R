data("jags_logit")

test_that("Simple model runs with mcmcCoefPlot", {
  
  expect_silent(mcmcCoefPlot(jags_logit))

})

test_that("mcmcCoefPlot works with jags_logit", {
  ## mcmc
  expect_silent(mcmcCoefPlot(coda::as.mcmc(jags_logit)[[1]]))
})

if (require("MCMCpack", quietly = TRUE)) {
  ## fitting the model with MCMCpack
  mcmcpack_linear <- MCMCpack::MCMCregress(Y ~ X, b0 = 0, B0 = 0.001,
                                           sigma.mu = 5, sigma.var = 10,
                                           data = list(X = rnorm(100),
                                                       Y = rnorm(100, 5, 5)),
                                           seed = 1)
  ## testing
  test_that("mcmcCoefPlot works with mcmcpack", {
    ## mcmc.list
    expect_silent(mcmcCoefPlot(mcmcpack_linear))
  })
}

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

pkgs <- c("runjags")

if (!all(sapply(pkgs, require, quietly = TRUE, character.only = TRUE))) {
  
  ## Generate an example runjags interactive fitted model
  ## formatting the data for jags
  datalist <- list(X = model.matrix(~ X1 * X2, sim_data_interactive),
                   Y = sim_data_interactive[, 3],
                   N = nrow(sim_data_interactive))
  
  ## creating jags model
  model <- "model { 
  for(i in 1 : N){ 
  Y[i] ~ dnorm(beta %*% X[i, ], tau);
  } 
  for(i in 1:4) {
  beta[i] ~ dnorm(0, 0.001)
  }
  tau ~ dexp(1)
}"

  ## fitting the model with runjags
  runjags_interactive <- runjags::run.jags(model = model, monitor = c("beta", "tau"),
                                           data = datalist, n.chains = 2, method = "rjags")
  
  ## Do the test
  test_that("mcmcReg works with runjags", {
    
    expect_match(mcmcReg(runjags_interactive), "-0.55; -0.24")
    
  })
  
  ## Do fail test
  test_that("mcmcCoefPlot works with multiple object types", {
    
    ## runjags
    expect_silent(mcmcCoefPlot(runjags_interactive))
    
  })
}


pkgs <- c("rjags", "R2WinBUGS")

if (!all(sapply(pkgs, require, quietly = TRUE, character.only = TRUE))) {
  
  ## Generate an example BUGS fitted model object
  data(LINE, package = "rjags")
  LINE$recompile()
  
  ## fitting the model with jags
  bugs_model <- rjags::coda.samples(LINE, c("alpha", "beta", "sigma"),
                                    n.iter = 1000)
  bugs_model <- R2WinBUGS::as.bugs.array(sims.array = as.array(bugs_model))
  
  test_that("mcmcCoefPlot works with bugs", {
    
    ## bugs
    expect_silent(mcmcCoefPlot(bugs_model))
    
  })
  
}