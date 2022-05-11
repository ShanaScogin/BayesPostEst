## data files used:
## jags_interactive.rds
## sim_data_interactive.rds
## jags_interactive_cat.rds
## sim_data_interactive_cat.rds

test_that("Simple model runs with mcmcMargEff", {
  
  jags_interactive <- readRDS("../testdata/jags_interactive.rds")
  fit <- jags_interactive
  
  sim_data_interactive <- readRDS("../testdata/sim_data_interactive.rds")
  
  ## using mcmcMargEff
  fit_me <- mcmcMargEff(mod = fit,
                        main = 'b[2]',
                        int = 'b[4]',
                        moderator = sim_data_interactive$X2,
                        plot = F)
  
  ## testing
  value <- fit_me[25, 3]
  check_against <- 0.519057
  expect_equal(value, check_against, tolerance = 1e-2)
  
})

test_that("Simple model runs with mcmcMargEff with arguments", {
  
  jags_interactive <- readRDS("../testdata/jags_interactive.rds")
  fit <- jags_interactive
  
  sim_data_interactive <- readRDS("../testdata/sim_data_interactive.rds")
  
  ## processing the data
  mcmc <- coda::as.mcmc(fit)
  
  ## using mcmcMargEff
  fit_me <- mcmcMargEff(mod = fit,
                        main = 'b[2]',
                        int = 'b[4]',
                        moderator = sim_data_interactive$X2,
                        pointest = 'median',
                        seq = 50,
                        ci = .9,
                        hpdi = T,
                        plot = F)
  
  ## testing
  value <- fit_me[37, 4]
  check_against <- 0.5145419
  expect_equal(value, check_against, tolerance = 1e-2)
  
})

test_that("Simple model runs with mcmcMargEff with plotting arguments", {
  
  jags_interactive <- readRDS("../testdata/jags_interactive.rds")
  fit <- jags_interactive
  
  sim_data_interactive <- readRDS("../testdata/sim_data_interactive.rds")
  
  ## using mcmcMargEff
  plot_me <- mcmcMargEff(mod = fit,
                         main = 'b[2]',
                         int = 'b[4]',
                         moderator = sim_data_interactive$X2,
                         plot = T,
                         xlab = 'Moderating Variable',
                         ylab = 'Marginal Effect of X1')
  
  ## testing
  expect_equal(plot_me$labels$y, 'Marginal Effect of X1')
  expect_equal(plot_me$labels$x, 'Moderating Variable')
  
})

test_that("Simple model runs with mcmcMargEff with categorical moderator", {
  
  jags_interactive_cat <- readRDS("../testdata/jags_interactive_cat.rds")
  fit <- jags_interactive_cat
  
  sim_data_interactive_cat <- readRDS("../testdata/sim_data_interactive_cat.rds")
  
  ## using mcmcMargEff
  fit_me <- mcmcMargEff(mod = fit,
                        main = 'b[2]',
                        int = 'b[4]',
                        moderator = sim_data_interactive_cat$X3,
                        plot = F,
                        xlab = 'Moderating Variable',
                        ylab = 'Marginal Effect of X1')
  
  ## testing
  value <- fit_me[4, 3]
  check_against <- -0.9342449
  expect_equal(value, check_against, tolerance = 1e-2)
  
})

