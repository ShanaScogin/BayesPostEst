## Combining mcmcLogitFD and mcmcProbitFD

#'@title First Differences of a Bayesian Logit or Probit model
#'@description R function to calculate first differences after a Bayesian logit or probit model 
#'@param formula A formula object, with the dependent variable on the
#'left of a ~ operator, and the independent variables on the right. If argument
#'\code{modelmatrix} is supplied, function defaults to that argument
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model. If argument
#'\code{modelmatrix} is supplied, function defaults to that argument
#'@param modelmatrix model matrix, including intercept. Include instead of \code{formula} and
#'\code{data} arguments. If all three are included, function defaults to modelmatrix.
#'Create with model.matrix(formula, data)
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

mcmcFD <- function(formula,
                   data,
                   modelmatrix,
                   mcmcout, 
                   link = "logit",
                   ci = c(0.025, 0.975),
                   percentiles = c(0.25, 0.75),
                   fullsims = FALSE) {
  
  # checking arguments
  if(!missing(modelmatrix)) {
    fd.mat <- matrix(NA, ncol = 3, nrow = ncol(modelmatrix) - 1)
  } else {
    if(missing(formula) | missing(data)) {
      stop("Please enter both the formula and data.")
    } else {
      modelmatrix <- model.matrix(formula = formula, data = data)
      fd.mat <- matrix(NA, ncol = 3, nrow = ncol(modelmatrix) - 1)
    }
  }
  
  
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
    X[, i] <- ifelse(length(unique(modelmatrix[, i])) == 2 & 
                     range(modelmatrix[, i]) == c(0, 1), c(0, 1), 
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
  
  if(fullsims == FALSE){
    return(fd.dat)
  }
  
  if(fullsims == TRUE){
    return(fd.full)
  }
  
}
