#'This function calculates predicted probabilities for "average" cases after a Bayesian 
#'logit or probit model. For an explanation of predicted probabilities for "average" cases,
#' see e.g. King, Tomz & Wittenberg (2000, <doi: 10.2307/2669316>)
#'@title Bayesian MCMC Predicted Probablities for the 'Average' Case
#'@description This function calculates predicted probabilities for "average" cases after a Bayesian logit 
#'or probit model. For an explanation of predicted probabilities for "average" cases,
# see e.g. King, Tomz & Wittenberg (2000) doi: 10.2307/2669316
#'@param model_matrix model matrix, including intercept. Create with model.matrix(formula, data)
#'@param mcmc_out posterior distributions of all coefficients
#   in matrix form - can easily be created from rstan, MCMCpack, R2jags, R2OpenBUGS, etc.
#'@param xcol column number of the explanatory variable for which to calculate 
#'associated Pr(y = 1)
#'@param xrange name of the vector with the range of relevant values of the 
#'explanatory variable for which to calculate associated Pr(y = 1)
#'@param link link: link function, character vector set to "logit" (default) or "probit"
#'@param ci the bounds of the credible interval. Default is 0.05 and 0.95. 
#'Enter as a vector, such as c(0.05, 0.95).
#'#'@return a matrix with 4 columns:
#'predictor: identical to x_range
#'median_pp: median predicted probability at given x
#'lower_pp: lower bound of credible interval of predicted probability at given x
#'upper_pp: upper bound of credible interval of predicted probability at given x
#'@examples
#' \donttest{
#'   ## simulating data
#'   set.seed(123456)
#'   b0 <- 0.2 # true value for the intercept
#'   b1 <- 0.5 # true value for first beta
#'   b2 <- 0.7 # true value for second beta
#'   n <- 500 # sample size
#'   X1 <- runif(n, -1, 1)
#'   X2 <- runif(n, -1, 1)
#'   Z <- b0 + b1 * X1 + b2 * X2
#'   pr <- 1 / (1 + exp(-Z)) # inv logit function
#'   Y <- rbinom(n, 1, pr) 
#'   data <- data.frame(cbind(X1, X2, Y))
#'   
#'   ## formatting the data for jags
#'   datjags <- as.list(data)
#'   datjags$N <- length(datjags$Y)
#'   
#'   ## creating jags model
#'   model <- function()  {
#'   
#'   for(i in 1:N){
#'     Y[i] ~ dbern(p[i])  ## Bernoulli distribution of y_i
#'     logit(p[i]) <- mu[i]    ## Logit link function
#'     mu[i] <- b[1] + 
#'       b[2] * X1[i] + 
#'       b[3] * X2[i]
#'   }
#'   
#'   for(j in 1:3){
#'     b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
#'   }
#'   
#'}
#' 
#' params <- c("b")
#' inits1 <- list("b" = rep(0, 3))
#' inits2 <- list("b" = rep(0, 3))
#' inits <- list(inits1, inits2)
#' 
#' ## fitting the model with R2jags
#' set.seed(123)
#' fit <- R2jags::jags(data = datjags, inits = inits, 
#'                     parameters.to.save = params, n.chains = 2, n.iter = 2000, 
#'                     n.burnin = 1000, model.file = model)
#' 
#' ### average value approach
#' xmat <- model.matrix(Y ~ X1 + X2, data = data)
#' mcmc <- coda::as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
#' X1_sim <- seq(from = min(datjags$X1),
#'               to = max(datjags$X1), 
#'               length.out = 10)
#' ave_prob <- mcmcAveProb(model_matrix = xmat,
#'                         mcmc_out = mcmc_mat,
#'                         xcol = 2,
#'                         xrange = X1_sim)
#' }
#'@export
#'
mcmcAveProb <- function(model_matrix, 
                        mcmc_out, 
                        xcol, 
                        xrange, 
                        link = "logit", 
                        ci = c(0.025, 0.975)){
  
  X <- matrix(rep(apply(X = model_matrix,
                        MARGIN = 2,
                        FUN = function(x) median(x)),
                  times = length(xrange)),
              nrow = length(xrange),
              byrow = TRUE)
  X[, xcol] <- xrange
  
  if(link == "logit"){
    logit_linpred <- t(X %*% t(mcmc_out))
    logit_pp <- exp(logit_linpred) / (1 + exp(logit_linpred)) # still seems fine
    pp <- logit_pp}
  
  if(link == "probit"){
    pp <- pnorm(t(X %*% t(mcmc_out)))
  }
  
  colnames(pp) <- as.character(xrange)
  longFrame <- reshape2::melt(pp, id.vars = Var2)
  
  pp_dat <- dplyr::summarize(dplyr::group_by(longFrame, Var2), 
                      median_pp = quantile(value, probs = 0.5), 
                      lower_pp = quantile(value, probs = ci[1]), 
                      upper_pp = quantile(value, probs = ci[2]))
  
  names(pp_dat) <- c("predictor", "median_pp", "lower_pp", "upper_pp")
  
  return(pp_dat)
}