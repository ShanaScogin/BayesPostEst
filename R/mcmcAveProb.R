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
#'@param x_col column number of the explanatory variable for which to calculate 
#'associated Pr(y = 1)
#'@param x_range_vec name of the vector with the range of relevant values of the 
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
#'   set.seed(123456)
#'   example for user goes here
#'   unit testing goes in testthat
#' }
#'@export
#'
mcmcAveProb <- function(model_matrix, 
                        mcmc_out, 
                        x_col, 
                        x_range_vec, 
                        link = "logit", 
                        ci = c(0.05, 0.95)){
  
  X <- matrix(rep(apply(X = model_matrix,
                        MARGIN = 2,
                        FUN = function(x) median(x)),
                  times = length(x_range_vec)),
              nrow = length(x_range_vec),
              byrow = TRUE)
  X[, x_col] <- x_range_vec
  
  if(link == "logit"){
    logit_linpred <- t(X %*% t(mcmc_out))
    logit_pp <- exp(logit_linpred) / (1 + exp(logit_linpred)) # still seems fine
    pp <- logit_pp}
  
  if(link == "probit"){
    pp <- pnorm(t(X %*% t(mcmc_out)))
  }
  
  colnames(pp) <- as.character(x_range_vec)
  longFrame <- reshape2::melt(pp, id.vars = Var2)
  
  pp_dat <- dplyr::summarize(dplyr::group_by(longFrame, Var2), 
                      median_pp = quantile(value, probs = 0.5), 
                      lower_pp = quantile(value, probs = ci[1]), 
                      upper_pp = quantile(value, probs = ci[2]))
  
  names(pp_dat) <- c("predictor", "median_pp", "lower_pp", "upper_pp")
  
  return(pp_dat)
}