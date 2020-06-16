#'@title First Differences of a Bayesian Logit or Probit model
#'@description R function to calculate first differences after a Bayesian logit or probit model.
#'First differences are a method to summarize effects across covariates. This quantity represents
#'the difference in predicted probabilities for each covariate for cases with low and high values 
#'of the respective covariate. For each of these differences, all other variables are held constant 
#'at their median. For more, see Long (1997, Sage Publications) and King, Tomz, and Wittenberg (2000, 
#'American Journal of Political Science 44(2): 347-361).
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
#'@param link type of generalized linear model; a character vector set to \code{"logit"} (default) 
#'or \code{"probit"}.
#'@param ci the bounds of the credible interval. Default is \code{c(0.025, 0.975)} for the 95\% 
#'credible interval.
#'@param percentiles values of each predictor for which the difference in Pr(y = 1) 
#'is to be calculated. Default is \code{c(0.25, 0.75)}, which will calculate the difference 
#'between Pr(y = 1) for the 25th percentile and 75th percentile of the predictor. For binary 
#'predictors, the function automatically calculates the difference between Pr(y = 1) 
#'for x = 0 and x = 1.
#'@param fullsims logical indicator of whether full object (based on all MCMC draws 
#'rather than their average) will be returned. Default is \code{FALSE}. 
#'@references 
#'\itemize{
#'\item King, Gary, Michael Tomz, and Jason Wittenberg. 2000. “Making the Most of Statistical 
#'Analyses: Improving Interpretation and Presentation.” American Journal of Political Science 
#'44 (2): 347–61. http://www.jstor.org/stable/2669316
#'\item Long, J. Scott. 1997. Regression Models for Categorical and Limited Dependent Variables. 
#'Thousand Oaks: Sage Publications
#'}
#'@return An object of class \code{mcmcFD}. If \code{fullsims = FALSE} (default),
#'a data frame with five columns:
#'\itemize{
#'\item median_fd: median first difference
#'\item lower_fd: lower bound of credible interval of the first difference
#'\item upper_fd: upper bound of credible interval of the first difference
#'\item VarName: name of the variable as found in \code{modelmatrix}
#'\item VarID: identifier of the variable, based on the order of columns in
#'\code{modelmatrix} and  \code{mcmcout}. Can be adjusted for plotting
#'}
#'If \code{fullsims = TRUE}, a matrix with as many columns as predictors in the model. Each row 
#'is the first difference for that variable based on one set of posterior draws. Column names are taken 
#'from the column names of \code{modelmatrix}.
#'
#'@examples
#' \dontshow{.old_wd <- setwd(tempdir())}
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
#' df <- data.frame(cbind(X1, X2, Y))
#' 
#' ## formatting the data for jags
#' datjags <- as.list(df)
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
#' xmat <- model.matrix(Y ~ X1 + X2, data = df)
#' mcmc <- coda::as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
#' object <- mcmcFD(modelmatrix = xmat,
#'                  mcmcout = mcmc_mat)
#' object
#' }
#' 
#' \dontshow{setwd(.old_wd)}
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
      pp <- plogis(t(X %*% t(mcmcout)))
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
  
  if (fullsims) {
    return(structure(fdfull, fullsims = TRUE, class = c("mcmcFD", "matrix")))
  } else {
    return(structure(fddat, fullsims = FALSE, class = c("mcmcFD", "data.frame")))
  }

}

#'@export
print.mcmcFD <- function(x, ...) {
  if (attr(x, "fullsims")) {
    print.table(x)
  } else {
    print.data.frame(x)
  }
}

#'@title Plot Method for First Differences from MCMC output 
#'@description The \code{plot} method for first differences generated from MCMC
#'output by \code{\link{mcmcFD}}. For more on this method, see Long
#'(1997, Sage Publications), and King, Tomz, and Wittenberg (2000, American
#'Journal of Political Science 44(2): 347-361). For a description of this type
#'of plot, see Figure 1 in Karreth (2018, International Interactions 44(3): 463-90).
#'@param x Output generated from \code{mcmcFD(..., full_sims = TRUE)}.
#'@param ROPE defaults to NULL. If not NULL, a numeric vector of length two, 
#'defining the Region of Practical Equivalence around 0. See Kruschke (2013, Journal of 
#'Experimental Psychology 143(2): 573-603) for more on the ROPE. 
#'@param ... optional arguments to \code{\link[ggplot2]{theme}} from \code{\link[ggplot2:ggplot2-package]{ggplot2}}.
#'@references 
#'\itemize{
#'\item Karreth, Johannes. 2018. “The Economic Leverage of International Organizations in Interstate Disputes.” 
#'International Interactions 44 (3): 463-90. https://doi.org/10.1080/03050629.2018.1389728.
#'\item King, Gary, Michael Tomz, and Jason Wittenberg. 2000. “Making the Most of Statistical 
#'Analyses: Improving Interpretation and Presentation.” American Journal of Political Science 
#'44 (2): 347–61. http://www.jstor.org/stable/2669316.
#'\item Kruschke, John K. 2013. “Bayesian Estimation Supersedes the T-Test.” Journal of 
#'Experimental Psychology: General 142 (2): 573–603. https://doi.org/10.1037/a0029146.
#'\item Long, J. Scott. 1997. Regression Models for Categorical and Limited Dependent Variables. 
#'Thousand Oaks: Sage Publications.
#'}
#'@return a density plot of the differences in probabilities. The plot is made with ggplot2 and can be
#'passed on as an object to customize. Annotated numbers show the percent of posterior draws with
#'the same sign as the median estimate (if \code{ROPE = NULL}) or on the same side of the 
#'ROPE as the median estimate (if \code{ROPE} is specified).
#'
#'@seealso \code{\link{mcmcFD}}
#'
#'@method plot mcmcFD
#'
#'@examples
#' \dontshow{.old_wd <- setwd(tempdir())}
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
#' df <- data.frame(cbind(X1, X2, Y))
#' 
#' ## formatting the data for jags
#' datjags <- as.list(df)
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
#' ## preparing data for mcmcFD()
#' xmat <- model.matrix(Y ~ X1 + X2, data = df)
#' mcmc <- coda::as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
#' 
#' ## plotting with mcmcFDplot()
#' full <- mcmcFD(modelmatrix = xmat,
#'                mcmcout = mcmc_mat,
#'                fullsims = TRUE)
#' plot(full)
#' 
#' }
#' 
#' \dontshow{setwd(.old_wd)}
#' 
#' @export
#'
plot.mcmcFD <- function(x,
                        ROPE = NULL,
                        ...) {
  
  if (!attr(x, "fullsims")) {
    stop("full simulations must be used to plot posterior distribution")
  }
  
  ROPE <- check_ROPE_argument(ROPE)
  
  ## multiply by 100 for percentage point change in output
  x <- x * 100
  
  # convert x to long data frame
  fd_dat <- tidyr::gather(as.data.frame(x))
  
  # create first plot
  
  if(!is.null(ROPE)) {
    fd_plot <- ggplot2::ggplot(data = fd_dat, aes(x = .data$value, y = .data$key)) + 
      ggplot2::geom_rect(xmin = ROPE[1], xmax = ROPE[2], ymin = 0, ymax = Inf, fill = "black") + 
      ggridges::stat_density_ridges(quantile_lines = TRUE, 
                                    quantiles = c(0.025, 0.5, 0.975),
                                    vline_color = "white") + 
      ggplot2::scale_x_continuous(labels = function(x) x * 100) + 
      ggplot2::xlab("Percentage point change in Pr(y = 1)") + 
      ggplot2::ylab("")
    
    # calculate area left/right of ROPE
    fd_outROPE <- apply(x, 2, 
                        function(x) ifelse(median(x) < 0, 
                                           sum(x < ROPE[1]) / length(x), 
                                           sum(x > ROPE[2]) / length(x)))
    fd_annotate <- data.frame(xpos = apply(x, 2, 
                                           function(x) ifelse(median(x) < 0, 
                                                              quantile(x, probs = 0.01) - 0.02, 
                                                              quantile(x, probs = 0.99) + 0.02)), 
                              ypos = as.factor(colnames(x)), 
                              outROPE = paste(round(fd_outROPE * 100, digits = 1), "%", sep = ""))
    
    # final plot
    fd_plot <- fd_plot + 
      geom_text(data = fd_annotate, aes(x = .data$xpos, y = .data$ypos, label = .data$outROPE), 
                color = "black", nudge_y = 0.1, size = 4) +
      ggplot2::theme(...)
  } else {
    fd_plot <- ggplot2::ggplot(data = fd_dat,
                               aes(x = .data$value, y = .data$key)) + 
      ggplot2::geom_vline(xintercept = 0) + 
      ggridges::stat_density_ridges(quantile_lines = TRUE, 
                                    quantiles = c(0.025, 0.5, 0.975),
                                    vline_color = "white") + 
      ggplot2::xlab("Percentage point change in Pr(y = 1)") + 
      ggplot2::ylab("")
    
    # calculate area left/right of 0
    fd_out0 <- apply(x, 2, 
                     function(x) ifelse(median(x) < 0, 
                                        sum(x < 0) / length(x), 
                                        sum(x > 0) / length(x)))
    fd_annotate <- data.frame(xpos = apply(x, 2, 
                                           function(x) ifelse(median(x) < 0, 
                                                              quantile(x, probs = 0.01) - 0.02, 
                                                              quantile(x, probs = 0.99) + 0.02)), 
                              ypos = as.factor(colnames(x)), 
                              out0 = paste(round(fd_out0 * 100, digits = 1), "%", sep = ""))
    
    # final plot
    fd_plot <- fd_plot + 
      ggplot2::geom_text(data = fd_annotate, 
                         aes(x = .data$xpos, y = .data$ypos, label = .data$out0), 
                         color = "black", nudge_y = 0.1, size = 4) +
      ggplot2::theme(...)
    
  }
  
  fd_plot
  
}


## mcmcFDplot - deprecated

#'@title Plot First Differences from MCMC output 
#'@description R function to plot first differences generated from MCMC output.
#'For more on this method, see the documentation for \code{mcmcFD()}, Long (1997, 
#'Sage Publications), and King, Tomz, and Wittenberg (2000, American Journal 
#'of Political Science 44(2): 347-361). For a description of this type of plot,
#'see Figure 1 in Karreth (2018, International Interactions 44(3): 463-90).
#'@param fdfull Output generated from \code{mcmcFD(..., full_sims = TRUE)}.
#'@param ROPE defaults to NULL. If not NULL, a numeric vector of length two, 
#'defining the Region of Practical Equivalence around 0. See Kruschke (2013, Journal of 
#'Experimental Psychology 143(2): 573-603) for more on the ROPE. 
#'@references 
#'\itemize{
#'\item Karreth, Johannes. 2018. “The Economic Leverage of International Organizations in Interstate Disputes.” 
#'International Interactions 44 (3): 463-90. https://doi.org/10.1080/03050629.2018.1389728.
#'\item King, Gary, Michael Tomz, and Jason Wittenberg. 2000. “Making the Most of Statistical 
#'Analyses: Improving Interpretation and Presentation.” American Journal of Political Science 
#'44 (2): 347–61. http://www.jstor.org/stable/2669316.
#'\item Kruschke, John K. 2013. “Bayesian Estimation Supersedes the T-Test.” Journal of 
#'Experimental Psychology: General 142 (2): 573–603. https://doi.org/10.1037/a0029146.
#'\item Long, J. Scott. 1997. Regression Models for Categorical and Limited Dependent Variables. 
#'Thousand Oaks: Sage Publications.
#'}
#'@return a density plot of the differences in probabilities. The plot is made with ggplot2 and can be
#'passed on as an object to customize. Annotated numbers show the percent of posterior draws with
#'the same sign as the median estimate (if \code{ROPE = NULL}) or on the same side of the 
#'ROPE as the median estimate (if \code{ROPE} is specified).
#'
#'@examples
#' \dontshow{.old_wd <- setwd(tempdir())}
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
#' df <- data.frame(cbind(X1, X2, Y))
#' 
#' ## formatting the data for jags
#' datjags <- as.list(df)
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
#' ## preparing data for mcmcFD()
#' xmat <- model.matrix(Y ~ X1 + X2, data = df)
#' mcmc <- coda::as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
#' 
#' ## plotting with mcmcFDplot()
#' full <- mcmcFD(modelmatrix = xmat,
#'                mcmcout = mcmc_mat,
#'                fullsims = TRUE)
#' # suppress deprecated warning for R check
#' suppressWarnings(mcmcFDplot(full))
#' 
#' }
#' 
#' \dontshow{setwd(.old_wd)}
#' 
#' @name mcmcFDplot-deprecated
#' @usage mcmcFDplot(fdfull, ROPE = NULL)
#' @seealso \code{\link{BayesPostEst-deprecated}}
#' @seealso \code{\link{mcmcFD}}
#' @keywords internal
#' 
#' @rdname BayesPostEst-deprecated
#' @section \code{mcmcFDplot}:
#' For \code{mcmcFDplot}, use \code{\link{plot.mcmcFD}}.
#' 
#' @export
#'

mcmcFDplot <- function(fdfull, 
                       ROPE = NULL){
  
  .Deprecated("plot.mcmcFD", package = "BayesPostEst")
  
  ROPE <- check_ROPE_argument(ROPE)
  
  fdfull <- fdfull * 100
  
  fd_dat <- tidyr::gather(as.data.frame(fdfull))
  
  # create first plot
  
  if(!is.null(ROPE)) {
    fd_plot <- ggplot2::ggplot(data = fd_dat, aes(x = .data$value, y = .data$key)) + 
      ggplot2::geom_rect(xmin = ROPE[1], xmax = ROPE[2], ymin = 0, ymax = Inf, fill = "black") + 
      ggridges::stat_density_ridges(quantile_lines = TRUE, 
                                    quantiles = c(0.025, 0.5, 0.975),
                                    vline_color = "white") + 
      ggplot2::scale_x_continuous(labels = function(x) x * 100) + 
      ggplot2::xlab("Percentage point change in Pr(y = 1)") + 
      ggplot2::ylab("")
    
    # calculate area left/right of ROPE
    fd_outROPE <- apply(fdfull, 2, 
                        function(x) ifelse(median(x) < 0, 
                                           sum(x < ROPE[1]) / length(x), 
                                           sum(x > ROPE[2]) / length(x)))
    fd_annotate <- data.frame(xpos = apply(fdfull, 2, 
                                           function(x) ifelse(median(x) < 0, 
                                                              quantile(x, probs = 0.01) - 0.02, 
                                                              quantile(x, probs = 0.99) + 0.02)), 
                              ypos = as.factor(colnames(fdfull)), 
                              outROPE = paste(round(fd_outROPE * 100, digits = 1), "%", sep = ""))
    
    # final plot
    fd_plot <- fd_plot + 
      geom_text(data = fd_annotate, aes(x = .data$xpos, y = .data$ypos, label = .data$outROPE), 
                color = "black", nudge_y = 0.1, size = 4)
  } else {
    fd_plot <- ggplot2::ggplot(data = fd_dat,
                               aes(x = .data$value, y = .data$key)) + 
      ggplot2::geom_vline(xintercept = 0) + 
      ggridges::stat_density_ridges(quantile_lines = TRUE, 
                                    quantiles = c(0.025, 0.5, 0.975),
                                    vline_color = "white") + 
      ggplot2::scale_x_continuous(labels = function(x) x*100) + 
      ggplot2::xlab("Percentage point change in Pr(y = 1)") + 
      ggplot2::ylab("")
    
    # calculate area left/right of 0
    fd_out0 <- apply(fdfull, 2, 
                     function(x) ifelse(median(x) < 0, 
                                        sum(x < 0) / length(x), 
                                        sum(x > 0) / length(x)))
    fd_annotate <- data.frame(xpos = apply(fdfull, 2, 
                                           function(x) ifelse(median(x) < 0, 
                                                              quantile(x, probs = 0.01) - 0.02, 
                                                              quantile(x, probs = 0.99) + 0.02)),
                              ypos = as.factor(colnames(fdfull)), 
                              out0 = paste(round(fd_out0 * 100, digits = 1), "%", sep = ""))
    
    # final plot
    fd_plot <- fd_plot + 
      ggplot2::geom_text(data = fd_annotate, 
                         aes(x = .data$xpos, y = .data$ypos, label = .data$out0), 
                         color = "black", nudge_y = 0.1, size = 4)
  }
  
  fd_plot
  
}