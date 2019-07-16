## Combining mcmcLogitFD and mcmcProbitFD

#'@title First Differences of a Bayesian Logit or Probit model
#'@description R function to calculate first differences after a Bayesian logit or probit model 
#'@param modelmatrix model matrix, including intercept. Create with model.matrix(formula, data)
#'@param mcmcout posterior distributions of all logit coefficients, 
#'in matrix form - can easily be created from rstan, MCMCpack, R2jags, etc.
#'@param link type of model. It is a character vector set to 
#'"logit" (default) or "probit"
#'@param ci the bounds of the credible interval. Default is \code{c(0.025, 0.975)}.
#'@param percentiles default is \code{c(0.25, 0.75)}
#'@param fullsims logical indicator of whether full object (based on all MCMC draws 
#'rather than average) will be returned. Default is \code{FALSE}
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
                   fullsims = FALSE) {
  
  fdmat <- matrix(NA, ncol = 3, nrow = ncol(modelmatrix) - 1)
  colnames(fdmat) <- c("Median", "Lower", "Upper")
  rownames(fdmat) <- colnames(modelmatrix)[-1]
  
  fdfull <- matrix(rep(NA),
                    ncol = ncol(modelmatrix) - 1,
                    nrow = nrow(mcmcout),
                    byrow = TRUE)
  colnames(fdfull) <- colnames(modelmatrix)[-1]
  
  for (i in 2:ncol(modelmatrix)){
    
    X <- matrix(rep(apply(X = modelmatrix,
                          MARGIN = 2,
                          FUN = function(x) median(x)),
                    times = 2),
                nrow = 2,
                byrow = TRUE)
    X[, i] <- ifelse(length(unique(modelmatrix[, i])) == 2 & 
                     range(modelmatrix[, i]) == c(0, 1), c(0, 1), 
                     quantile(modelmatrix[, i], probs = percentiles))
    
    # X[, i] <- quantile(modelmatrix[, i], probs = percentiles)
    
    Xb <- t(X %*% t(mcmcout))
    pp <- exp(Xb) / (1 + exp(Xb))
    
    fd <- pp[, 2] - pp[, 1]
    
    fdmat[i-1, 1] <- quantile(fd, probs = c(0.5))
    fdmat[i-1, 2] <- quantile(fd, probs = c(ci[1]))
    fdmat[i-1, 3] <- quantile(fd, probs = c(ci[2]))
    
    fdfull[, i-1] <- fd
    
  }
  
  fd.dat <- as.data.frame(fdmat)
  fd.dat$VarName <- rownames(fdmat)
  fd.dat$VarID <- row(fdmat)[, 1]
  
  if(fullsims == FALSE){
    return(fd.dat)
  }
  
  if(fullsims == TRUE){
    return(fdfull)
  }
  
}
