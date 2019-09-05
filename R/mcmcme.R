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
#' @param plot logical indicating whether to return a `ggplot` object or the
#' underlying tidy DataFrame.
#'
#' @return a `ggplot` object or a tidy DataFrame.
#' @export
#'
#' @examples
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
  }
  
  ## calculate marginal effect for mean
  if (hpdi) {
    marg_ci <- t(apply(marg, 2, HDInterval::hdi, credMass = ci))
  } else {
    marg_ci<- t(apply(marg, 2, quantile, probs = c(.5 - ci/2, .5 + ci/2)))
  }

  ## create dataframe for plotting
  marg_gg <- data.frame(mod = mod_range, pe = marg_pe,
                        lo = marg_ci[, 1], hi = marg_ci[, 2])

  mep <- ggplot(data = marg_gg, aes(x = mod_range, y = pe, ymin = lo, ymax = hi)) +
    geom_ribbon(alpha = .25) +
    geom_hline(yintercept = 0, lty = 2, color = 'gray40', lwd = .5) +
    geom_line()

  ## return plot
  if (plot) mep else marg_gg

}
