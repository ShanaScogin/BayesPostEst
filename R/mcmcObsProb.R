#'This function calculates predicted probabilities for 
#'"observed" cases after a Bayesian logit or probit model
#'following Hanmer and Kalkan (2013, American Journal of 
#'Political Science 57(1): 263-277)
#'@title Bayesian MCMC Observed Values Predicted Probablities
#'@description Implements R function to calculate the predicted probabilities
#'for "observed" cases after a Bayesian logit or probit model, following
#'Hanmer & Kalkan (2013) <doi: 10.1111/j.1540-5907.2012.00602.x>.
#'@param modelmatrix model matrix, including intercept (if the intercept is among the
#'parameters of interest from the model). Create with model.matrix(formula, data).
#'Note: the order of columns in the model matrix must correspond to the order of columns 
#'in the matrix of posterior draws in the the \code{mcmcout} argument. See the \code{mcmcout}
#'argument for more
#'@param mcmcout posterior distributions of all logit coefficients, 
#'in matrix form. This can be created from rstan, MCMCpack, R2jags, etc. and transformed
#'into a matrix using the function as.mcmc() from the coda package for \code{jags} class
#'objects, as.matrix() from base R for \code{mcmc}, \code{mcmc.list}, \code{stanreg}, and 
#'\code{stanfit} class objects, and \code{object$sims.matrix} for \code{bugs} class objects.
#'Note: the order of columns in this matrix must correspond to the order of columns 
#'in the model matrix. One can do this by examining the posterior distribution matrix and listing the 
#'variables in the order of this matrix when creating the matrix model. A useful function for sorting as 
#'you create the matrix of posterior distributions is \code{mixedsort()} fom the gtools package
#'@param xcol column number of the posterior draws (\code{mcmcout}) and model matrices 
#'that corresponds to the explanatory variable for which to calculate associated Pr(y = 1).
#'Note that the columns in these matrices must match
#'@param xrange name of the vector with the range of relevant values of the 
#'explanatory variable for which to calculate associated Pr(y = 1)
#'@param xinterest semi-optional argument. Name of the explanatory variable for which 
#'to calculate associated Pr(y = 1). If \code{xcol} is supplied, this is not needed. 
#'If both are supplied, the function defaults to \code{xcol} and this argument is ignored
#'@param link type of model. It is a character vector set to "logit" (default) or "probit"
#'@param ci the bounds of the credible interval. Default is \code{c(0.05, 0.95)}.
#'@param fullsims logical indicator of whether full object (based on all MCMC draws 
#'rather than average) will be returned. Default is \code{FALSE}. A note: The longer 
#'\code{xrange} is, the larger the full output will be if \code{TRUE} is selected.
#'@references Hanmer, M. J., & Ozan Kalkan, K. (2013). Behind the curve: Clarifying 
#'the best approach to calculating predicted probabilities and marginal effects from 
#'limited dependent variable models. American Journal of Political Science, 57(1), 
#'263-277. https://doi.org/10.1111/j.1540-5907.2012.00602.x
#'@return a matrix with 4 columns:
#'\itemize{
#'\item x: identical to x_range
#'\item median_pp: median predicted probability at given x
#'\item lower_pp: lower bound of credible interval of predicted probability at given x
#'\item upper_pp: upper bound of credible interval of predicted probability at given x
#'}
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
#' library(R2jags)
#' set.seed(123)
#' fit <- jags(data = datjags, inits = inits, 
#'           parameters.to.save = params, n.chains = 2, n.iter = 2000, 
#'           n.burnin = 1000, model.file = model)
#' 
#' ### observed value approach
#' library(coda)
#' xmat <- model.matrix(Y ~ X1 + X2, data = data)
#' mcmc <- as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
#' X1_sim <- seq(from = min(datjags$X1),
#'               to = max(datjags$X1), 
#'               length.out = 10)
#' obs_prob <- mcmcObsProb(modelmatrix = xmat,
#'                         mcmcout = mcmc_mat,
#'                         xrange = X1_sim,
#'                         xcol = 2)
#' }
#'@export
#'
mcmcObsProb <- function(modelmatrix,
                        mcmcout, 
                        xcol, 
                        xrange, 
                        xinterest,
                        link = "logit", 
                        ci = c(0.025, 0.975),
                        fullsims = FALSE){
  
  # checking arguments
  if(missing(xcol) & missing(xinterest)) {
    stop("Please enter a column number or name of your variable of interest)")
  }
  if(!missing(xcol) & !missing(xinterest)) {
    message("Both xcol and xinterest were supplied by user. Function defaults to xcol")
  }
  if(!missing(xinterest)) {
    if(!(xinterest %in% variable.names(modelmatrix)))
      stop("Variable name does not match any in the matrix. Please enter another.")
  }
  
  X <- matrix(rep(t(modelmatrix), length(xrange)), 
              ncol = ncol(modelmatrix), byrow = TRUE )
  colnames(X) <- variable.names(modelmatrix)
  if(!missing(xcol)) {
    X[, xcol] <- sort(rep(xrange, times = nrow(X) / length(xrange)))
  } else {
    X[ , grepl( xinterest , variable.names( X ) ) ] <- 
      sort(rep(xrange, times = nrow(X) / length(xrange)))
  }
  
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
  
  pp_dat <- dplyr::tibble(x = xrange,
                   median_pp = median_pp,
                   lower_pp = lower_pp,
                   upper_pp = upper_pp)
  
  if(fullsims == FALSE){
    return(pp_dat) # pp_dat was created by summarizing longFrame
  }
  
  if(fullsims == TRUE){
    longFrame <- reshape2::melt(pp_mat, id.vars = .data::Var2)
    names(longFrame) <- c("Iteration", "x", "pp")
    return(longFrame) 
  }
  
}
