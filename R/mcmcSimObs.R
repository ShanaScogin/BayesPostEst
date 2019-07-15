#'This function calculates predicted probabilities for 
#'"observed" cases after a Bayesian logit or probit model
#'following Hanmer & Kalkan (2013) <doi: 10.1111/j.1540-5907.2012.00602.x>.
#'
#'@title Bayesian MCMC Observed Values Predicted Probablities
#'@description Implements R function to calculate the predicted probabilities
#'for "observed" cases after a Bayesian logit or probit model, following
#'Hanmer & Kalkan (2013) <doi: 10.1111/j.1540-5907.2012.00602.x>.
#'@param modelmatrix model matrix, including intercept. Create with 
#'model.matrix(formula, data)
#'@param mcmcout posterior distributions of all coefficients in matrix 
#'form - can easily be created from rstan, MCMCpack, R2jags, R2OpenBUGS, etc.
#'@param xcol column number of the explanatory variable for which to calculate 
#'associated Pr(y = 1)
#'@param xrange name of the vector with the range of relevant values of the 
#'explanatory variable for which to calculate associated Pr(y = 1)
#'@param link link function, character vector set to "logit" (default) or "probit"
#'@param ci the bounds of the credible interval. Default is 0.05 and 0.95. 
#'Enter as a vector, such as c(0.05, 0.95).
#'@references Hanmer, M. J., & Ozan Kalkan, K. (2013). Behind the curve: Clarifying 
#'the best approach to calculating predicted probabilities and marginal effects from 
#'limited dependent variable models. American Journal of Political Science, 57(1), 
#'263-277.
#'@return An object
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
#' ### observed value approach with mcmcSimObs
#' obs_prob_sim <- mcmcObsProb(formula = Y ~ X1 + X2,
#'                         data = data,
#'                         xinterest = c("X1"),
#'                         mcmcfit = fit)
#' }
#'@export

mcmcSimObs <- function(formula,
                        data,
                        xinterest,
                        mcmcfit,
                        simout = 10, 
                        link = "logit", 
                        ci = c(0.025, 0.975)){
  
  # formula argument
  if(missing(formula)) {
      stop("Please enter the formula")
  }
  
  if(missing(data)) {
    stop("Please enter data")
  } else{
    modelmatrix <- model.matrix(object = formula, data = data)
  }

  # range of x variable of interest argument
  if(missing(xinterest)) {
    stop("Please enter your variable of interest")
  } else {
    var <- data[ , grepl( xinterest , names( data ) ) ]
    xrange <- seq(from = min(var),
                  to = max(var),
                  length.out = simout)
  }
  
  # fit variable
  if(missing(mcmcfit)) {
    stop("Please enter the mcmc fit output")
  } else {
    mcmcout <- as.matrix(coda::as.mcmc(fit))[, 1:ncol(modelmatrix)]
  }

  X <- matrix(rep(t(modelmatrix), length(xrange)), 
              ncol = ncol(modelmatrix), byrow = TRUE )
  colnames(X) <- variable.names(modelmatrix)
  X[ , grepl( xinterest , variable.names( X ) ) ] <- 
    sort(rep(xrange, times = nrow(X) / length(xrange)))
  
  if(link == "logit"){
    logit_linpred <- t(X %*% t(mcmcout))
    logit_pp <- exp(logit_linpred) / (1 + exp(logit_linpred)) # still seems fine
    pp <- logit_pp
  }
  
  if(link == "probit"){
    pp <- pnorm(t(X %*% t(mcmcout)))
  }
  
  
  # emptry matrix for PPs
  pp_mat <- matrix(NA, nrow = nrow(mcmcout), ncol = length(xrange))
  
  # indices
  pp_mat_lowerindex <- 1 + (0:(length(xrange) - 1) * nrow(modelmatrix))
  pp_mat_upperindex <- nrow(modelmatrix) + (0:(length(xrange) - 1) * 
                                               nrow(modelmatrix))
  
  
  # fill matrix with PPs, one for each value of the predictor of interest
  for(i in 1:length(xrange)){
    pp_mat[, i] <- apply(X = pp[, 
                                c(pp_mat_lowerindex[i]:pp_mat_upperindex[i])], 
                         MARGIN = 1, FUN = function(x) mean(x))
  }
  
  median_pp <- apply(X = pp_mat, MARGIN = 2, function(x) quantile(x, probs = c(0.5)))
  lower_pp <- apply(X = pp_mat, MARGIN = 2, function(x) quantile(x, probs = ci[1]))
  upper_pp <- apply(X = pp_mat, MARGIN = 2, function(x) quantile(x, probs = ci[2]))
  
  pp_dat <- dplyr::tibble(predictor = xrange,
                   median_pp = median_pp,
                   lower_pp = lower_pp,
                   upper_pp = upper_pp)
  
  return(pp_dat)
}
