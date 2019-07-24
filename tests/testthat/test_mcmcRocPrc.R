test_that("Simple model runs with mcmcRocPrc", {
  
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
  
  ## processing the data
  mm <- model.matrix(Y ~ X1 + X2, data = data)
  xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = data))
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]
  
  ## using mcmcRocPrc
  fit_sum <- mcmcRocPrc(modelmatrix = mm,
                        modelframe = xframe,
                        mcmcout = mcmc_mat,
                        curves = TRUE,
                        fullsims = FALSE)
  
  ## testing
  value <- fit_sum$prc_dat[156, 2]
  check_against <- c(0.658)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  value_roc <- fit_sum$area_under_roc
  check_against_roc <- c(0.627)
  expect_equal(round(as.numeric(value_roc), 2), round(check_against_roc, 2))
  
  value_prc <- fit_sum$area_under_prc
  check_against_prc <- c(0.621)
  expect_equal(round(as.numeric(value_prc), 2), round(check_against_prc, 2))
  
  ## using mcmcRocPrc with full draws
  fit_full <- mcmcRocPrc(modelmatrix = mm,
                            modelframe = xframe,
                            mcmcout = mcmc_mat,
                            curves = TRUE,
                            fullsims = TRUE)
  
  ## testing
  value_area_under_roc <- unlist(lapply(fit_full, '[[', 1))[986]
  check_against_full_roc <- c(0.626)
  expect_equal(round(as.numeric(value_area_under_roc), 2), 
               round(check_against_full_roc, 2))
  
  
  
  value_area_under_prc <- unlist(lapply(fit_full, '[[', 2))[965]
  check_against_full_prc <- c(0.620)
  expect_equal(round(as.numeric(value_area_under_prc), 2), 
               round(check_against_full_prc, 2))
})

test_that("Simple model runs with mcmcRocPrc Full", {
  skip_on_cran()
  ## this test is longer than CRAN allows
  
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
  
  ## processing the data
  mm <- model.matrix(Y ~ X1 + X2, data = data)
  xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = data))
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]

  ## using mcmcRocPrc with full draws
  fit_full <- mcmcRocPrc(modelmatrix = mm,
                         modelframe = xframe,
                         mcmcout = mcmc_mat,
                         curves = TRUE,
                         fullsims = TRUE)
  
  ## testing
  value_area_under_roc <- unlist(lapply(fit_full, '[[', 1))[986]
  check_against_full_roc <- c(0.626)
  expect_equal(round(as.numeric(value_area_under_roc), 2), 
               round(check_against_full_roc, 2))
  
  
  
  value_area_under_prc <- unlist(lapply(fit_full, '[[', 2))[965]
  check_against_full_prc <- c(0.620)
  expect_equal(round(as.numeric(value_area_under_prc), 2), 
               round(check_against_full_prc, 2))
})
