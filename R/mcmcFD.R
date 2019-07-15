## Combining mcmcLogitFD and mcmcProbitFD

#'@title First Differences of a Bayesian Logit or Probit model
#'@description R function to calculate first differences after a Bayesian logit or probit model 
#'@param modelmatrix model matrix, including intercept. Create with model.matrix(formula, data)
#'@param mcmcout posterior distributions of all logit coefficients, 
#'in matrix form - can easily be created from rstan, MCMCpack, R2jags, etc.
#'@param link type of model. It is a character vector set to 
#'"logit" (default) or "probit"
#'@param ci the bounds of the credible interval. Default is 0.05 and 0.95. 
#'Enter as a vector, such as c(0.05, 0.95).
#'@param percentiles default is c(0.25, 0.75)
#'@param full_sims logical indicator of whether full object will be returned.
#'Default is FALSE.
#'@return output
#'@examples
#' \donttest{
#'   set.seed(123456)
#' }
#'@export

mcmcFD <- function(modelmatrix,
                   mcmcout, 
                   link = "logit",
                   ci = c(0.025, 0.975),
                   percentiles = c(0.25, 0.75),
                   full_sims = FALSE){
  
  fd.mat <- matrix(NA, ncol = 3, nrow = ncol(modelmatrix) - 1)
  colnames(fd.mat) <- c("Median", "Lower", "Upper")
  rownames(fd.mat) <- colnames(modelmatrix)[-1]
  
  fd.full <- matrix(rep(NA),
                    ncol = ncol(modelmatrix) - 1,
                    nrow = nrow(mcmcout),
                    byrow = TRUE)
  colnames(fd.full) <- colnames(modelmatrix)[-1]
  
  for (i in 2:ncol(modelmatrix)){
    
    X <- matrix(rep(apply(X = modelmatrix,
                          MARGIN = 2,
                          FUN = function(x) median(x)),
                    times = 2),
                nrow = 2,
                byrow = TRUE)
    X[, i] <- ifelse(length(unique(modelmatrix[, i])) == 2 & range(modelmatrix[, i]) == c(0, 1), c(0, 1), 
                     quantile(modelmatrix[, i], probs = percentiles))
    
    # X[, i] <- quantile(modelmatrix[, i], probs = percentiles)
    
    Xb <- t(X %*% t(mcmcout))
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