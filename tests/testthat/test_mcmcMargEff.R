test_that("Simple model runs with mcmcMargEff", {
  
  data("jags_interactive")
  fit <- jags_interactive
  
  data("sim_data_linear")
  
  ## processing the data
  mcmc <- coda::as.mcmc(fit)
  
  ## using mcmcMargEff
  fit_me <- mcmcMargEff(mod = fit,
                        main = 'b[2]',
                        int = 'b[4]',
                        moderator = sim_data_linear$X2,
                        plot = F)
  
  ## testing
  value <- fit_me[25, 3]
  check_against <- 0.56214
  expect_equal(value, check_against, tolerance = 1e-2)
  
})

test_that("Simple model runs with mcmcMargEff with arguments", {
  
  data("jags_interactive")
  fit <- jags_interactive
  
  data("sim_data_linear")
  
  ## processing the data
  mcmc <- coda::as.mcmc(fit)
  
  ## using mcmcMargEff
  fit_me <- mcmcMargEff(mod = fit,
                        main = 'b[2]',
                        int = 'b[4]',
                        moderator = sim_data_linear$X2,
                        pointest = 'median',
                        ci = .9,
                        hpdi = T,
                        plot = F)
  
  ## testing
  value <- fit_me[87, 4]
  check_against <- 0.4656877
  expect_equal(value, check_against, tolerance = 1e-2)
  
})
