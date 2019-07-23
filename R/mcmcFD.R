#'@title First Differences of a Bayesian Logit or Probit model
#'@description R function to calculate first differences after a Bayesian logit or probit model.
#'First differences are a method to summarize effects across covariates. This quantity represents
#'the difference in predicted probabilities for each covariate for cases with low and high values 
#'of the respective covariate. For each of these differences, all other variables are held constant 
#'at their median. For more, see Long (1997, Sage Publications) and King, Tomz, and Wittenberg (2000, 
#'American Journal of Political Science 44(2): 347-361)
#'@param modelmatrix model matrix, including intercept (if the intercept is among the
#'parameters of interest from the model). Create with model.matrix(formula, data).
#'Note: the order of columns in the model matrix must correspond to the order of columns 
#'in the matrix of posterior draws in the \code{mcmcout} argument. See the \code{mcmcout}
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
#'@param link type of model. It is a character vector set to \code{"logit"} (default) or \code{"probit"}
#'@param ci the bounds of the credible interval. Default is \code{c(0.025, 0.975)}.
#'@param percentiles default is \code{c(0.25, 0.75)}
#'@param fullsims logical indicator of whether full object (based on all MCMC draws 
#'rather than average) will be returned. Default is \code{FALSE}
#'@references 
#'\itemize{
#'\item King, Gary, Michael Tomz, and Jason Wittenberg. 2000. “Making the Most of Statistical 
#'Analyses: Improving Interpretation and Presentation.” American Journal of Political Science 
#'44 (2): 347–61. http://www.jstor.org/stable/2669316
#'\item Kruschke, John K. 2013. “Bayesian Estimation Supersedes the T-Test.” Journal of 
#'Experimental Psychology: General 142 (2): 573–603. https://doi.org/10.1037/a0029146
#'\item Long, J. Scott. 1997. Regression Models for Categorial and Limited Dependent Variables. 
#'Thousand Oaks: Sage Publications
#'}
#'@return an object of class \code{matrix} with the first differences for each
#'covariate
#'@examples
#' \donttest{
#' ## simulating data
#' set.seed(123456)
#' b0 <- 0.2 # true value for the intercept
#' b1 <- 0.5 # true value for first beta
#' b2 <- 0.7 # true value for second beta
#' n <- 500 # sample size
#' X1 <- runif(n, -1, 1)
#' X2 <- runif(n, -1, 1)
#' Z <- b0 + b1 * X1 + b2 * X2
#' pr <- 1 / (1 + exp(-Z)) # inv logit function
#' Y <- rbinom(n, 1, pr) 
#' data <- data.frame(cbind(X1, X2, Y))
#' 
#' ## formatting the data for jags
#' datjags <- as.list(data)
#' datjags$N <- length(datjags$Y)
#' 
#' ## creating jags model
#' model <- function()  {
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
#' }
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
#' ## running function with logit
#' xmat <- model.matrix(Y ~ X1 + X2, data = data)
#' mcmc <- coda::as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
#' object <- mcmcFD(modelmatrix = xmat,
#'                  mcmcout = mcmc_mat)
#' object
#' }
#'@export
#'
mcmcFD <- function(modelmatrix,
                   mcmcout, 
                   link = "logit",
                   ci = c(0.025, 0.975),
                   percentiles = c(0.25, 0.75),
                   fullsims = FALSE) {
  
  if(missing(modelmatrix) | missing(mcmcout)){
    stop("Please enter required arguments.")
  }

  fdmat <- matrix(NA, ncol = 3, nrow = ncol(modelmatrix) - 1)
  colnames(fdmat) <- c("median_fd", "lower_fd", "upper_fd")
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

    if(link == "logit") {
      Xb <- t(X %*% t(mcmcout))
    pp <- exp(Xb) / (1 + exp(Xb))
    } else if (link == "probit") {
      pp <- pnorm(t(X %*% t(mcmcout)))
    } else {
      stop("Please enter valid link argument.")
    }


    fd <- pp[, 2] - pp[, 1]

    fdmat[i-1, 1] <- quantile(fd, probs = c(0.5))
    fdmat[i-1, 2] <- quantile(fd, probs = c(ci[1]))
    fdmat[i-1, 3] <- quantile(fd, probs = c(ci[2]))

    fdfull[, i-1] <- fd

  }

  fddat <- as.data.frame(fdmat)
  fddat$VarName <- rownames(fdmat)
  fddat$VarID <- row(fdmat)[, 1]

  if(fullsims == FALSE){
    return(fddat)
  }

  if(fullsims == TRUE){
    return(fdfull)
  }
  
}
