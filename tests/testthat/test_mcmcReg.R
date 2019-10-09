test_that("Simple model runs with mcmcReg", {
  
  data("jags_logit")
  fit <- jags_logit
  
  expect_silent(mcmcReg(fit))

  expect_silent(mcmcReg(fit, pars = c('b')))

  expect_silent(object3 <- mcmcReg(fit, pars = c('b'),
     custom.coef.names = c('Variable 1',
                           'Variable 2',
                           'Variable 3')))
  
  expect_silent(mcmcReg(fit, coefnames = c('Variable 1',
                                           'Variable 2',
                                           'Variable 3',
                                           'deviance')))
  
  expect_silent(mcmcReg(fit, pointest = 'median', hpdi = T))
  
  expect_silent(mcmcReg(fit, gof = 1234, gofnames = 'Fake Fit Statistic'))
  
  expect_silent(mcmcReg(fit, format = 'html', file = 'table'))
  
  ## remove table.html to avoid non-standard file note on check
  if (file.exists('table.html')) file.remove('table.html')

})


