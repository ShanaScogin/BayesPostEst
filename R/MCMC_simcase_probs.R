# R function to calculate predicted probabilities for 
# "average" cases after a Bayesian logit or probit model 
# Johannes Karreth

###########################################
# NOTE: This function is not fully tested
# Use with caution! Feedback appreciated.
###########################################

# For an explanation of predicted probabilities for "average" cases,
# see e.g. King, Tomz & Wittenberg (2000)
# doi: 10.2307/2669316

# model_matrix: model matrix, including intercept. Create with model.matrix(formula, data)
# mcmc_out: posterior distributions of all coefficients
#   in matrix form - can easily be created from rstan, MCMCpack, R2jags, R2OpenBUGS, etc.
# x_col: column number of the explanatory variable for which to calculate associated Pr(y = 1)
# x_range_vec: name of the vector with the range of relevant values of the 
#   explanatory variable for which to calculate associated Pr(y = 1)
# link: link function, character vector set to "logit" (default) or "probit"
# lower: lower percentile (default: 5th) for credible interval of predicted probabilities
# upper: upper percentile (default: 95th) for credible interval of predicted probabilities

# Output: a matrix with 4 columns:
# predictor: identical to x_range
# median_pp: median predicted probability at given x
# lower_pp: lower bound of credible interval of predicted probability at given x
# upper_pp: upper bound of credible interval of predicted probability at given x

MCMC_simcase_probs <- function(model_matrix, mcmc_out, x_col, x_range_vec, link = "logit", lower = 0.05, upper = 0.95){
  
  require(dplyr); require(reshape2)
  
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
  longFrame <- melt(pp, id.vars = Var2)
  
  pp_dat <- summarize(group_by(longFrame, Var2), 
                      median_pp = quantile(value, probs = 0.5), 
                      lower_pp = quantile(value, probs = lower), 
                      upper_pp = quantile(value, probs = upper))
  
  names(pp_dat) <- c("predictor", "median_pp", "lower_pp", "upper_pp")
  
  return(pp_dat)
}