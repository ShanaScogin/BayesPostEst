test_that("Simple model runs with mcmcReg", {
  
  data("jags_logit")
  fit <- jags_logit
  
  expect_silent(mcmcReg(fit))

  expect_silent(mcmcReg(fit, pars = c('b')))

  expect_silent(object3 <- mcmcReg(fit, pars = c('b'),
     custom.coef.names = c(
                      'Variable 1',
                      'Variable 2',
                      'Variable 3')))
  
  expect_silent(mcmcReg(fit, custom.coef.names = c(
                       'Variable 1',
                       'Variable 2',
                       'Variable 3',
                       'deviance')))
  

})


