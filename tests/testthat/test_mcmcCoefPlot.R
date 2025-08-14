## packages used
## rjags
## MCMCpack
## R2WinBUGS
## runjags

## data files used:
## jags_logit.rds
## LINE from package = "rjags"
## sim_data_interactive

test_that("Simple model runs with mcmcCoefPlot", {
  
  testthat::skip_if_not_installed("rjags")
  
  #jags_logit <- readRDS("../testdata/jags_logit.rds")
  jags_logit <- readRDS(file.path(TESTDATA_DIR, "jags_logit.rds"))
  
  expect_silent(mcmcCoefPlot(jags_logit))
  
})

test_that("mcmcCoefPlot works with jags_logit", {
  
  testthat::skip_if_not_installed("rjags")
  
  #jags_logit <- readRDS("../testdata/jags_logit.rds")
  jags_logit <- readRDS(file.path(TESTDATA_DIR, "jags_logit.rds"))
  
  expect_silent(mcmcCoefPlot(coda::as.mcmc(jags_logit)[[1]]))
})



test_that("mcmcCoefPlot works with mcmcpack", {
    
    testthat::skip_if_not_installed("MCMCpack")
    
    ## fitting the model with MCMCpack
    mcmcpack_linear <- MCMCpack::MCMCregress(Y ~ X, b0 = 0, B0 = 0.001,
                                             sigma.mu = 5, sigma.var = 10,
                                             data = list(X = rnorm(100),
                                                         Y = rnorm(100, 5, 5)),
                                             seed = 1)
    
  expect_silent(mcmcCoefPlot(mcmcpack_linear))
})

test_that("Simple model runs with mcmcCoefPlot with arguments", {
  
  testthat::skip_if_not_installed("rjags")
  
  #jags_logit <- readRDS("../testdata/jags_logit.rds")
  jags_logit <- readRDS(file.path(TESTDATA_DIR, "jags_logit.rds"))
  
  ## running some expect silent tests for jags_logit
  expect_silent(mcmcCoefPlot(jags_logit))
  expect_silent(mcmcCoefPlot(jags_logit, pars = 'b', regex = T))
  expect_silent(mcmcCoefPlot(jags_logit, pars = 'b', ci = .9, hpdi = T, regex = T))
  
  ## test against a value
  med_df <- mcmcCoefPlot(jags_logit, pars = 'b', pointest = 'median', plot = F, regex = T)
  value <- med_df[2, 1]
  check_against <- 0.5273031
  expect_equal(value, check_against, tolerance = 0.1)
  
  ## running one more for regex = FALSE
  expect_silent(mcmcCoefPlot(jags_logit, pars = c('b[1]', 'b[2]'), regex = FALSE))
})

test_that("Simple model runs with mcmcCoefPlot with sorting", {
  
  testthat::skip_if_not_installed("rjags")
  
  #jags_logit <- readRDS("../testdata/jags_logit.rds")
  jags_logit <- readRDS(file.path(TESTDATA_DIR, "jags_logit.rds"))
  
  med_df <- mcmcCoefPlot(jags_logit, pars = 'b', sort = T, plot = F, regex = T)
  value <- med_df[3, 1]
  check_against <- 0.6335488
  expect_equal(value, check_against, tolerance = 0.1)
  
})

test_that("mcmcCoefPlot errors work", {
  
  testthat::skip_if_not_installed("rjags")
  
  #jags_logit <- readRDS("../testdata/jags_logit.rds")
  jags_logit <- readRDS(file.path(TESTDATA_DIR, "jags_logit.rds"))
  
  expect_error(mcmcCoefPlot(jags_logit, pointest = 'man'))
  expect_error(mcmcCoefPlot(jags_logit, hpdi = 2))
})

###############

test_that("mcmcCoefPlot works with runjags", {
  
  testthat::skip_if_not_installed("runjags")
  
  #sim_data_interactive <- readRDS("../testdata/sim_data_interactive.rds")
  sim_data_interactive <- readRDS(file.path(TESTDATA_DIR, "sim_data_interactive.rds"))
  
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
  suppressWarnings(runjags_interactive <- runjags::run.jags(model = model, 
                                           monitor = c("beta", "tau"),
                                           data = datalist, 
                                           n.chains = 2, 
                                           method = "rjags"))
  
  
  ## runjags
  expect_silent(mcmcCoefPlot(runjags_interactive))
})

test_that("mcmcCoefPlot works with bugs", {
  
  testthat::skip_if_not_installed(c("rjags", "R2WinBUGS"))
  
  data(LINE, package = "rjags")
  LINE$recompile()
  
  ## fitting the model with jags
  bugs_model <- rjags::coda.samples(LINE, c("alpha", "beta", "sigma"),
                                    n.iter = 1000)
  bugs_model <- R2WinBUGS::as.bugs.array(sims.array = as.array(bugs_model))
  
  expect_silent(mcmcCoefPlot(bugs_model))
})
