## Combining mcmcLogitFD and mcmcProbitFD

#'@title First Differences of a Bayesian Logit or Probit model
#'@description R function to calculate first differences after a Bayesian logit or probit model 
#'@param model_matrix model matrix, including intercept. Create with model.matrix(formula, data)
#'@param mcmc_out posterior distributions of all logit coefficients, 
#'in matrix form - can easily be created from rstan, MCMCpack, R2jags, etc.
#'@param ci the bounds of the credible interval
#'@param percentiles tbd
#'@param full_sims tbd
#'@return output
#'@examples
#' \donttest{
#'   set.seed(123456)
#'   example for user goes here
#'   unit testing goes in testthat
#' }
#'@export

mcmcFD <- function(model_matrix,
                   mcmc_out, 
                   link = "logit",
                   ci = c(0.05, 0.95), ## need to standardize this
                   percentiles = c(0.25, 0.75),
                   full_sims = FALSE){
  
  fd.mat <- matrix(NA, ncol = 3, nrow = ncol(model_matrix) - 1)
  colnames(fd.mat) <- c("Median", "Lower", "Upper")
  rownames(fd.mat) <- colnames(model_matrix)[-1]
  
  fd.full <- matrix(rep(NA),
                    ncol = ncol(model_matrix) - 1,
                    nrow = nrow(mcmc_out),
                    byrow = TRUE)
  colnames(fd.full) <- colnames(model_matrix)[-1]
  
  for (i in 2:ncol(model_matrix)){
    
    X <- matrix(rep(apply(X = model_matrix,
                          MARGIN = 2,
                          FUN = function(x) median(x)),
                    times = 2),
                nrow = 2,
                byrow = TRUE)
    X[, i] <- ifelse(length(unique(model_matrix[, i])) == 2 & range(model_matrix[, i]) == c(0, 1), c(0, 1), 
                     quantile(model_matrix[, i], probs = percentiles))
    
    # X[, i] <- quantile(model_matrix[, i], probs = percentiles)
    
    Xb <- t(X %*% t(mcmc_out))
    pp <- exp(Xb) / (1 + exp(Xb))
    
    fd <- pp[, 2] - pp[, 1]
    
    fd.mat[i-1, 1] <- quantile(fd, probs = c(0.5))
    fd.mat[i-1, 2] <- quantile(fd, probs = c(ci[1]))
    fd.mat[i-1, 3] <- quantile(fd, probs = c(ci[2]))
    
    fd.full[, i-1] <- fd
    
  }
  
  fd.dat <- as.data.frame(fd.mat)
  fd.dat$VarName <- rownames(fd.mat)
  fd.dat$VarID <- row(fd.mat)[, 1]
  
  if(full_sims == FALSE){
    return(fd.dat)
  }
  
  if(full_sims == TRUE){
    return(fd.full)
  }
  
}