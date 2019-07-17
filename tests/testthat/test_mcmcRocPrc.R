## need to get the actual tests in this

test_that("Simple model runs with mcmcAveProb", {

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
  
  ## fitting with stan
  library("rstanarm")
  m <- rstanarm::stan_glm(Y ~ X1 + X2,
                data = data,
                family = binomial(link = "logit"))
  
  # using mcmcRocPrc with median draws
  
  fit_sum <- mcmcRocPrc(sims = m,
                        modelframe = model.frame(m),
                        fullsims = FALSE)
  
  fit_sum$area_under_roc
  plot(x = fit_sum$roc_dat$x, 
       y = fit_sum$roc_dat$y, 
       type = "l", 
       main = "ROC")
  abline(a = 0, b = 1)
  
  fit_sum$area_under_prc
  plot(x = fit_sum$prc_dat$x, 
       y = fit_sum$prc_dat$y, 
       type = "l", 
       main = "PRC")
  
  # using mcmcRocPrc with full draws
  
  fit_full <- mcmcRocPrc(sims = m,
                         modelframe = model.frame(m),
                         fullsims = TRUE)
  
  # works but this is slooooow!
  
  # area under roc:
  area_under_roc <- unlist(lapply(fit_full, '[[', 1))
  area_under_prc <- unlist(lapply(fit_full, '[[', 2))

})
