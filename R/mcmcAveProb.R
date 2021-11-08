#'This function calculates predicted probabilities for "average" cases after a Bayesian 
#'logit or probit model. For an explanation of predicted probabilities for "average" cases,
#'see e.g. King, Tomz & Wittenberg (2000, American Journal of Political Science 44(2): 347-361)
#'@title Predicted Probabilities using Bayesian MCMC estimates for the "Average" Case
#'@description This function calculates predicted probabilities for "average" cases after 
#'a Bayesian logit or probit model. As "average" cases, this function calculates the median
#'value of each predictor. For an explanation of predicted probabilities for 
#'"average" cases, see e.g. King, Tomz & Wittenberg (2000, American Journal of 
#'Political Science 44(2): 347-361).
#'@param modelmatrix model matrix, including intercept (if the intercept is among the
#'parameters estimated in the model). Create with model.matrix(formula, data).
#'Note: the order of columns in the model matrix must correspond to the order of columns 
#'in the matrix of posterior draws in the \code{mcmcout} argument. See the \code{mcmcout}
#'argument for more.
#'@param mcmcout posterior distributions of all logit coefficients, 
#'in matrix form. This can be created from rstan, MCMCpack, R2jags, etc. and transformed
#'into a matrix using the function as.mcmc() from the coda package for \code{jags} class
#'objects, as.matrix() from base R for \code{mcmc}, \code{mcmc.list}, \code{stanreg}, and 
#'\code{stanfit} class objects, and \code{object$sims.matrix} for \code{bugs} class objects.
#'Note: the order of columns in this matrix must correspond to the order of columns 
#'in the model matrix. One can do this by examining the posterior distribution matrix and sorting the 
#'variables in the order of this matrix when creating the model matrix. A useful function for sorting 
#'column names containing both characters and numbers as 
#'you create the matrix of posterior distributions is \code{mixedsort()} from the gtools package.
#'@param xcol column number of the posterior draws (\code{mcmcout}) and model matrices 
#'that corresponds to the explanatory variable for which to calculate associated Pr(y = 1).
#'Note that the columns in these matrices must match.
#'@param xrange name of the vector with the range of relevant values of the 
#'explanatory variable for which to calculate associated Pr(y = 1).
#'@param xinterest semi-optional argument. Name of the explanatory variable for which 
#'to calculate associated Pr(y = 1). If \code{xcol} is supplied, this is not needed. 
#'If both are supplied, the function defaults to \code{xcol} and this argument is ignored.
#'@param link type of generalized linear model; a character vector set to \code{"logit"} 
#'(default) or \code{"probit"}.
#'@param ci the bounds of the credible interval. Default is \code{c(0.025, 0.975)} for the 95\% 
#'credible interval.
#'@param fullsims logical indicator of whether full object (based on all MCMC draws 
#'rather than their average) will be returned. Default is \code{FALSE}. Note: The longer 
#'\code{xrange} is, the larger the full output will be if \code{TRUE} is selected.
#'@references King, Gary, Michael Tomz, and Jason Wittenberg. 2000. “Making the Most 
#'of Statistical Analyses: Improving Interpretation and Presentation.” American Journal 
#'of Political Science 44 (2): 347–61. http://www.jstor.org/stable/2669316
#'@return if \code{fullsims = FALSE} (default), a tibble with 4 columns:
#'\itemize{
#'\item x: value of variable of interest, drawn from \code{xrange}
#'\item median_pp: median predicted Pr(y = 1) when variable of interest is set to x, 
#'holding all other predictors to average (median) values
#'\item lower_pp: lower bound of credible interval of predicted probability at given x
#'\item upper_pp: upper bound of credible interval of predicted probability at given x
#'}
#'if \code{fullsims = TRUE}, a tibble with 3 columns:
#'\itemize{
#'\item Iteration: number of the posterior draw
#'\item x: value of variable of interest, drawn from \code{xrange}
#'\item pp: average predicted Pr(y = 1) when variable of interest is set to x, holding all other predictors to average (median) values
#'}
#'@examples
#' \dontshow{.old_wd <- setwd(tempdir())}
#' \donttest{
#' if (interactive()) {
#'   ## simulating data
#'   set.seed(123)
#'   b0 <- 0.2 # true value for the intercept
#'   b1 <- 0.5 # true value for first beta
#'   b2 <- 0.7 # true value for second beta
#'   n <- 500 # sample size
#'   X1 <- runif(n, -1, 1)
#'   X2 <- runif(n, -1, 1)
#'   Z <- b0 + b1 * X1 + b2 * X2
#'   pr <- 1 / (1 + exp(-Z)) # inv logit function
#'   Y <- rbinom(n, 1, pr) 
#'   df <- data.frame(cbind(X1, X2, Y))
#'   
#'   ## formatting the data for jags
#'   datjags <- as.list(df)
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
#'          parameters.to.save = params, n.chains = 2, n.iter = 2000, 
#'          n.burnin = 1000, model.file = model)
#' 
#' ### average value approach
#' library(coda)
#' xmat <- model.matrix(Y ~ X1 + X2, data = df)
#' mcmc <- as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
#' X1_sim <- seq(from = min(datjags$X1),
#'               to = max(datjags$X1), 
#'               length.out = 10)
#' ave_prob <- mcmcAveProb(modelmatrix = xmat,
#'                         mcmcout = mcmc_mat,
#'                         xrange = X1_sim, 
#'                         xcol = 2)
#' }
#' }
#' 
#' \dontshow{setwd(.old_wd)}
#'@export
#'
mcmcAveProb <- function(modelmatrix, 
                        mcmcout, 
                        xcol, 
                        xrange, 
                        xinterest,
                        link = "logit", 
                        ci = c(0.025, 0.975),
                        fullsims = FALSE){
  
  # checking arguments
  if(missing(xcol) & missing(xinterest)) {
    stop("Please enter a column number or name of your variable of interest.)")
  }
  if(!missing(xcol) & !missing(xinterest)) {
    message("Both xcol and xinterest were supplied by user. Function defaults to xcol")
  }
  if(!missing(xinterest)) {
    if(!(xinterest %in% variable.names(modelmatrix)))
      stop("Variable name does not match any in the matrix. Please enter another.")
  }
  
  if(missing(modelmatrix) | missing(mcmcout) | missing(xrange)) {
    stop("Please enter modelmatrix, mcmcout, and xrange arguments")
  }
  
  X <- matrix(rep(apply(X = modelmatrix,
                        MARGIN = 2,
                        FUN = function(x) median(x)),
                  times = length(xrange)),
              nrow = length(xrange),
              byrow = TRUE)
  colnames(X) <- variable.names(modelmatrix)
  if(!missing(xcol)) {
    X[, xcol] <- xrange
  } else {
    X[ , grepl( xinterest , variable.names( X ) ) ] <- xrange
  }
  
  if(link == "logit"){
    pp <- plogis(t(X %*% t(mcmcout)))
  }
  
  if(link == "probit"){
    pp <- pnorm(t(X %*% t(mcmcout)))
  }
  
  colnames(pp) <- as.character(xrange)
  longFrame <- reshape2::melt(pp)
  
  pp_dat <- dplyr::summarize(dplyr::group_by(longFrame, .data$Var2), 
                      median_pp = quantile(.data$value, probs = 0.5), 
                      lower_pp = quantile(.data$value, probs = ci[1]), 
                      upper_pp = quantile(.data$value, probs = ci[2]))
  
  names(pp_dat) <- c("x", "median_pp", "lower_pp", "upper_pp")
  
  if(fullsims == FALSE){
    return(pp_dat) # pp_dat was created by summarizing longFrame
  }
  
  if(fullsims == TRUE){
    names(longFrame) <- c("Iteration", "x", "pp")
    return(longFrame) 
  }
  
}