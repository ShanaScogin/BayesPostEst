data("jags_logit")
fit <- jags_logit

test_that("Simple model runs with mcmcReg", {
  
  expect_silent(mcmcReg(fit))
  
  expect_silent(mcmcReg(fit, pars = c('b[1]', 'b[3]')))
  
  expect_silent(mcmcReg(fit, pars = c('b[1]', 'b[3]'),
                        coefnames = c('beta 1', 'beta 3')))
  
  expect_silent(mcmcReg(fit, pars = c('b'), coefnames = paste('beta', 1:3),
                        regex = T))
  
  expect_silent(mcmcReg(fit, pars = c('b'), regex = T))
  
  expect_silent(mcmcReg(fit, pars = c('b\\[1\\]', 'b\\[3\\]'), regex = T))

  expect_silent(object3 <- mcmcReg(fit, pars = c('b'),
     custom.coef.names = c('Variable 1',
                           'Variable 2',
                           'Variable 3'), regex = T))
  
  expect_silent(mcmcReg(fit, coefnames = c('Variable 1',
                                           'Variable 2',
                                           'Variable 3',
                                           'deviance')))
  
  expect_silent(mcmcReg(fit, gof = 1234, gofnames = 'Fake Fit Statistic'))
  
  expect_silent(mcmcReg(fit, format = 'html', file = 'table'))
  
  ## remove table.html to avoid non-standard file note on check
  if (file.exists('table.html')) file.remove('table.html')

})

test_that("mcmcReg works with hpdi", {
  
  expect_match(mcmcReg(fit, pointest = 'median', hpdi = T),
               "667.30; 675.03")
  
  expect_match(mcmcReg(fit, pointest = 'median', hpdi = T, format = 'html'),
               "667.30; 675.03")
  
})

test_that("mcmcReg works with custom coef names", {
  
  expect_match(mcmcReg(fit, coefnames = c('beta 1','beta 2', 'beta 3', 'deviance')),
               "beta 1")
  
  expect_error(mcmcReg(list(fit, fit), coefnames = c('beta 1','beta 2',
                                                     'beta 3', 'deviance')),
               regexp = "number of models does not match number of custom coefficient vectors")
  
})

test_that("mcmcReg works with standard deviation", {
  
  expect_match(mcmcReg(fit, sd = T), "\\$\\(0.16\\)\\$")
  expect_match(mcmcReg(fit, sd = T, format = 'html'), ">\\(0.16\\)<")
  
})

test_that("mcmcReg works with pr direction", {
  
  expect_match(mcmcReg(fit, pr = T), "\\$0.89\\$")
  expect_match(mcmcReg(fit, pr = T, format = 'html'), ">0.89<")
  
})

test_that("mcmcReg works with filenames", {
  
  expect_silent(mcmcReg(fit, sd = T, file = 'tab.tex'))
  expect_silent(mcmcReg(fit, sd = T, format = 'html', file = 'tab.html'))
  
})