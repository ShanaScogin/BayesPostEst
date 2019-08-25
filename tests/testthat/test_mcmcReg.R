test_that("Models run with mcmcReg", {
  
  options(digits = 10)
  library(datasets)
  
  # simple linear model
  fit1 <- brm::brm(mpg ~ cyl + disp + hp, data = mtcars,
              family = gaussian())
  mcmcReg(fit1, pars = c('b_Intercept', 'b'),
  custom.coef.map = list('b_cyl' = 'Cylinders',
                         'b_disp' = 'Displacement',
                         'b_hp' = 'Horsepower',
                         'b_Intercept' = '(Constant)'))

  # random effects linear model
  fit2 <- brm(mpg ~ cyl + disp + hp + (1 | gear),
              data = mtcars, family = gaussian())
  mcmcreg(fit2, pars = c('b_Intercept', 'b'))

})
