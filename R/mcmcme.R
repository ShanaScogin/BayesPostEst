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
#' @param point.est a character indicating whether to use the mean or median for
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
mcmcme <- function(mod, main, int, moderator, point.est = 'mean', seq = 100,
                   ci = .95, plot = T) {

  ## check for stanfit
  if (inherits(mod, 'stanfit')) {
    ## coefficients for independent and interactive effect
    samps <- rstan::extract(mod, pars = c(main, int))
  }

  ## check for brmsfit
  if (inherits(mod, 'brmsfit')) {
    ## coefficients for independent and interactive effect
    samps <- rstan::extract(mod$fit, pars = c(main, int))
  }

  ## expand moderating variable to range of values
  mod_range <- seq(min(moderator), max(moderator), length.out = seq)

  ## compute marginal effect for each sample
  marg <- rep(samps[[1]], seq) + samps[[2]] %o% mod_range

  ## calculate marginal effect for mean
  marg_mean <- apply(marg, 2, mean)

  ## calculate marginal effect for median and ci
  marg_med <- t(apply(marg, 2, quantile, probs = c(.5 - ci/2, .5,
                                                   .5 + ci/2)))

  ## create dataframe for plotting
  marg_gg <- data.frame(mod = mod_range, mean = marg_mean, median = marg_med[, 2],
                        lo = marg_med[, 1], hi = marg_med[, 3])

  ## use mean for point estimate
  if (point.est == 'mean') {

    mep <- ggplot(data = marg_gg, aes(x = mod_range, y = mean, ymin = lo, ymax = hi)) +
      geom_ribbon(alpha = .25) +
      geom_hline(yintercept = 0, lty = 2, color = 'gray40', lwd = .5) +
      geom_line()

  }

  ## use median for point esimate
  if (point.est == 'median') {

    mep <- ggplot(data = marg_gg, aes(x = mod_range, y = median, ymin = lo, ymax = hi)) +
      geom_ribbon(alpha = .25) +
      geom_hline(yintercept = 0, lty = 2, color = 'gray40', lwd = .5) +
      geom_line()

  }

  ## return plot
  if (plot == T) mep else marg_gg

}
