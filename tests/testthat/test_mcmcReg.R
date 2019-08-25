test_that("Models run with mcmcReg", {
  
  skip_on_cran()
  ## this test is longer than CRAN allows
  
  options(digits = 10)
  library(datasets)
  library(brms)
  
  # simple linear model
  fit1 <- brm(mpg ~ cyl + disp + hp, data = mtcars,
              family = gaussian())
  mcmcReg(fit1, pars = c('b_Intercept', 'b'),
          custom.coef.map = list('b_cyl' = 'Cylinders',
                         'b_disp' = 'Displacement',
                         'b_hp' = 'Horsepower',
                         'b_Intercept' = '(Constant)'))

  # random effects linear model
  fit2 <- brm(mpg ~ cyl + disp + hp + (1 | gear),
              data = mtcars, family = gaussian())
  mcmcReg(fit2, pars = c('b_Intercept', 'b'))

})
