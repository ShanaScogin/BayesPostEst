#' Marginal Effects Plots for MCMC Output
#'
#' Marginal effects plots for MCMC output using `ggplot2`
#'
#' @param mod a `stanfit` object.
#' @param main a character with the name of the parameter of interest in the
#' interaction term.
#' @param int a character with the name of the moderating parameter in the
#' interaction term.
#' @param moderator a vector of values that the moderating parameter takes on
#' in the data.
#' @param pointest a character indicating whether to use the mean or median for
#' point estimates in the plot.
#' @param seq a numeric giving the number of moderator values used to generate
#' the marginal effects plot.
#' @param ci a scalar indicating the confidence level of the uncertainty intervals.
#' @param hpdi a logical indicating whether to use highest posterior density intervals
#' or equal tailed credible intervals to capture uncertainty.
#' @param plot logical indicating whether to return a `ggplot` object or the
#' underlying tidy DataFrame.
#'
#' @return a `ggplot` object or a tidy DataFrame.
#'
#' @examples
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
#'
#' ## linear model data
#' Y_linear <- rnorm(n, Z, 1)
#' data <- data.frame(cbind(X1, X2, Y = Y_linear))
#' 
#' ## formatting the data for jags
#' datjags <- as.list(data)
#' datjags$N <- length(datjags$Y)
#' 
#' ## creating jags model
#' model <- function()  {
#'   
#'   for(i in 1:N){
#'     Y[i] ~ dnorm(mu[i], sigma)  ## Bernoulli distribution of y_i
#'     
#'     mu[i] <- b[1] + 
#'       b[2] * X1[i] + 
#'       b[3] * X2[i] +
#'       b[4] * X1[i] * X2[i]
#'     
#'   }
#'   
#'   for(j in 1:4){
#'     b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
#'   }
#'   
#'   sigma ~ dexp(1)
#'   
#' }
#' 
#' params <- c("b")
#' inits1 <- list("b" = rep(0, 4))
#' inits2 <- list("b" = rep(0, 4))
#' inits <- list(inits1, inits2)
#' 
#' ## fitting the model with R2jags
#' set.seed(123)
#' fit <- R2jags::jags(data = datjags, inits = inits, 
#'                     parameters.to.save = params, n.chains = 2, n.iter = 2000, 
#'                     n.burnin = 1000, model.file = model)
#'
#' mcmcMargEff(mod = fit,
#' main = 'b[2]',
#' int = 'b[4]',
#' moderator = sim_data_linear$X2,
#' plot = F)
#' }
#' 
#' @export
#' 
mcmcMargEff <- function(mod, main, int, moderator, pointest = 'mean', seq = 100,
                        ci = .95, hpdi = F, plot = T) {

  ## pull in unexported functions from other packages
  ## other options for future versions might include lifting this and adding authors as copr holders
  runjags.as.mcmc.list.runjags = getFromNamespace("as.mcmc.list.runjags", "runjags")
  if (inherits(mod, what = c("jags", "rjags"))) {
    samps <- as.matrix(coda::as.mcmc(mod))
  }
  if (inherits(mod, what = "bugs")) {
    samps <- mod$sims.matrix
  }
  if (inherits(mod, what = "runjags")) {
    samps <- as.matrix(runjags.as.mcmc.list.runjags(mod))
  }
  if (inherits(mod, what = c("mcmc", "mcmc.list", "stanfit", "stanreg",
                             "brmsfit"))) {
    samps <- as.matrix(mod)
  }
  
  samps <- samps[, c(main, int)]

  ## expand moderating variable to range of values
  mod_range <- seq(min(moderator), max(moderator), length.out = seq)

  ## compute marginal effect for each sample
  marg <- rep(samps[, 1], seq) + samps[, 2] %o% mod_range
  
  if (pointest == 'mean') {
    marg_pe <- apply(marg, 2, mean)
  } else if (pointest == 'median') {
    marg_pe <- apply(marg, 2, median)
  } else {
    stop("pointest must be either 'mean' or 'median'")
  }
  
  ## calculate marginal effect for mean
  if (!hpdi) {
    marg_ci<- t(apply(marg, 2, quantile, probs = c(.5 - ci/2, .5 + ci/2)))
  } else if (hpdi) {
    marg_ci <- t(apply(marg, 2, HDInterval::hdi, credMass = ci))
  } else {
    stop("hpdi must be either true or false")
  }

  ## create dataframe for plotting
  marg_gg <- data.frame(mod = mod_range, pe = marg_pe,
                        lo = marg_ci[, 1], hi = marg_ci[, 2])

  mep <- ggplot2::ggplot(data = marg_gg, aes(x = mod_range, y = pe, ymin = lo, ymax = hi)) +
    ggplot2::geom_ribbon(alpha = .25) +
    ggplot2::geom_hline(yintercept = 0, lty = 2, color = 'gray40', lwd = .5) +
    ggplot2::geom_line()

  ## return plot
  if (plot) mep else marg_gg

}
