test_that("Simple model runs with mcmcAveProb", {
  skip_on_cran()
  ## this test is longer than CRAN allows

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
  
  ## using mcmcRocPrc with median draws
  fit_sum <- mcmcRocPrc(sims = m,
                        modelframe = model.frame(m),
                        fullsims = FALSE)
  
  ## testing
  value <- fit_sum$prc_dat[156, 2]
  check_against <- c(0.658)
  expect_equal(round(as.numeric(value), 3), check_against)
  
  value_roc <- fit_sum$area_under_roc
  check_against_roc <- c(0.627)
  expect_equal(round(as.numeric(value_roc), 3), check_against_roc)
  
  value_prc <- fit_sum$area_under_prc
  check_against_prc <- c(0.621)
  expect_equal(round(as.numeric(value_prc), 3), check_against_prc)
  
  ## using mcmcRocPrc with full draws
  fit_full <- mcmcRocPrc(sims = m,
                         modelframe = model.frame(m),
                         fullsims = TRUE)
  
  ## testing
  value_area_under_roc <- unlist(lapply(fit_full, '[[', 1))[986]
  check_against_full_roc <- c(0.616)
  expect_equal(round(as.numeric(value_area_under_roc), 3), 
               check_against_full_roc)
  
  
  
  value_area_under_prc <- unlist(lapply(fit_full, '[[', 2))[965]
  check_against_full_prc <- c(0.618)
  expect_equal(round(as.numeric(value_area_under_prc), 3), 
               check_against_full_prc)
})
