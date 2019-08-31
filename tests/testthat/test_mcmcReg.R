test_that("Simple model runs with mcmcReg", {
  
  options(digits = 10)
  
  ## simulating data
  set.seed(123456)
  b0 <- 0.2 # true value for the intercept
  b1 <- 0.5 # true value for first beta
  b2 <- 0.7 # true value for second beta
  n <- 500 # sample size
  X1 <- runif(n, -1, 1)
  X2 <- runif(n, -1, 1)
  Z <- b0 + b1 * X1 + b2 * X2
  pr <- 1 / (1 + exp(-Z)) # inv logit function
  Y <- rbinom(n, 1, pr)
  data <- data.frame(cbind(X1, X2, Y))
  
  ## formatting the data for jags
  datjags <- as.list(data)
  datjags$N <- length(datjags$Y)
  
  ## creating jags model
  model <- function()  {
    
    for(i in 1:N){
      Y[i] ~ dbern(p[i])  ## Bernoulli distribution of y_i
      logit(p[i]) <- mu[i]    ## Logit link function
      mu[i] <- b[1] +
        b[2] * X1[i] +
        b[3] * X2[i]
    }
    
    for(j in 1:3){
      b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
    }
    
  }
  
  params <- c("b")
  inits1 <- list("b" = rep(0, 3))
  inits2 <- list("b" = rep(0, 3))
  inits <- list(inits1, inits2)
  
  ## fitting the model with R2jags
  set.seed(123)
  fit <- R2jags::jags(data = datjags, inits = inits,
                      parameters.to.save = params, n.chains = 2, n.iter = 2000,
                      n.burnin = 1000, model.file = model)
  
  expect_silent(mcmcReg(fit))

  object2 <- mcmcReg(fit, pars = c('b'))

  object3 <- mcmcReg(fit, pars = c('b'),
     custom.coef.names = list(
                      'b[1]' = 'Cylinders',
                      'b[2]' = 'Displacement',
                      'b[3]' = 'Horsepower')
                     )
  

})

test_that("Simple model runs with mcmcReg", {
  
  skip("skipping test for brms")

  skip_on_cran()
  ## this test is longer than CRAN allows

  library(datasets)
  library(brms)

  # simple linear model
  fit1 <- brm(mpg ~ cyl + disp + hp, data = mtcars,
              family = gaussian())
  mcmcReg(fit1, pars = c('b_Intercept', 'b'),
          custom.coef.names = list('b_Intercept' = '(Constant)',
                           'b_cyl' = 'Cylinders',
                           'b_disp' = 'Displacement',
                           'b_hp' = 'Horsepower'))

})

