test_that("Simple model runs with mcmcObsRob", {
  
  ## simulating some data
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
  
  ### observed value approach
  xmat <- model.matrix(Y ~ X1 + X2, data = data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  X1_sim <- seq(from = min(datjags$X1),
                to = max(datjags$X1), 
                length.out = 10)
  obs_prob <- mcmcObsProb(modelmatrix = xmat,
                          mcmcout = mcmc_mat,
                          xcol = 2,
                          xrange = X1_sim)
  
  value <- obs_prob[1, 1]
  check_against <- c(-0.998)
  expect_equal(round(as.numeric(value), 3), check_against)
  
  obs_prob2 <- mcmcObsProb(modelmatrix = xmat,
                          mcmcout = mcmc_mat,
                          xinterest = "X1",
                          xcol = 2,
                          xrange = X1_sim)
  
  
  ## Compare to Johannes' previous function
  # devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/MCMC_observed_probs.R")
  # prot_obs_prob <- MCMC_observed_probs(model_matrix = xmat,
  #                                      mcmc_out = mcmc_mat,
  #                                      x_col = 2,
  #                                      x_range_vec = X1_sim,
  #                                      upper = 0.975,
  #                                      lower = 0.025)
  # 
  
})
