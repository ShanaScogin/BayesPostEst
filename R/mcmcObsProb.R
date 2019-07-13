#'This function calculates predicted probabilities for 
#'"observed" cases after a Bayesian logit or probit model
#'following Hanmer & Kalkan (2013) <doi: 10.1111/j.1540-5907.2012.00602.x>.
#'
#'@title Bayesian MCMC Observed Values Predicted Probablities
#'@description Implements R function to calculate the predicted probabilities
#'for "observed" cases after a Bayesian logit or probit model, following
#'Hanmer & Kalkan (2013) <doi: 10.1111/j.1540-5907.2012.00602.x>.
#'@param model_matrix model matrix, including intercept. Create with 
#'model.matrix(formula, data)
#'@param mcmc_out posterior distributions of all coefficients in matrix 
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
#'   set.seed(123456)
#'   example for user goes here
#'   unit testing goes in testthat
#' }
#'@export

mcmcObsProb <- function(model_matrix,
                        mcmc_out, 
                        xcol, ## I think should we rename this or allow a var name
                        xrange, 
                        link = "logit", 
                        ci = c(0.05, 0.95)){
  
  X <- matrix(rep(t(model_matrix), length(xrange)), 
              ncol = ncol(model_matrix), byrow = TRUE )
  X[, xcol] <- sort(rep(xrange, times = nrow(X) / length(xrange)))
  
  if(link == "logit"){
    logit_linpred <- t(X %*% t(mcmc_out))
    logit_pp <- exp(logit_linpred) / (1 + exp(logit_linpred)) # still seems fine
    pp <- logit_pp
  }
  
  if(link == "probit"){
    pp <- pnorm(t(X %*% t(mcmc_out)))
  }
  
  
  # emptry matrix for PPs
  pp_mat <- matrix(NA, nrow = nrow(mcmc_out), ncol = length(xrange))
  
  # indices
  pp_mat_lowerindex <- 1 + (0:(length(xrange) - 1) * nrow(model_matrix))
  pp_mat_upperindex <- nrow(model_matrix) + (0:(length(xrange) - 1) * 
                                               nrow(model_matrix))
  
  
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
