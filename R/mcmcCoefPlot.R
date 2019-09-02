#' Coefficient Plots for MCMC Output
#'
#' Coefficient plots for MCMC output using `ggplot2`
#'
#' @param mod a `stanfit` object.
#' @param pars a scalar or vector of the parameters you wish to include in the table.
#' `stanfit` objects can use either the individual parameter names, or the
#' names of the indexed parameter to retrieve the entire parameter e.g.
#' `pars = 'beta'` will return `beta[1]`, `beta[2]`, and `beta[3]` for a Stan
#' model with a three element beta parameter.
#' @param ci a scalar indicating the confidence level of the uncertainty intervals.
#' @param plot logical indicating whether to return a `ggplot` object or the
#' underlying tidy DataFrame.
#'
#' @return a `ggplot` object or a tidy DataFrame.
#' @export
#'
#' @examples
mcmccp <- function(mod, pars, ci, plot = T) {

  ## check for stanfit
  if (inherits(mod, 'stanfit')) {

    ## extract summary from model object
    coefs <- data.frame(rstan::summary(mod, pars = pars,
                                       probs = c(.5 - ci/2, .5,
                                                 .5 + ci/2))$summary[, c(1, 4:6)])

  }

  ## check for brmsfit
  if (inherits(mod, 'brmsfit')) {

    ## extract summary from model object
    coefs <- data.frame(rstan::summary(mod$fit, pars = pars,
                                       probs = c(.5 - ci/2, .5,
                                                 .5 + ci/2))$summary[, c(1, 4:6)])

  }

  ## create variable of predictor name for plotting
  coefs$variable <- factor(rownames(coefs), levels = rownames(coefs))

  ## rename credible interval columns for dynamic referencing in ggplot
  colnames(coefs)[c(2, 4)] <- c('low', 'high')

  ## coefficient plot
  cp <- ggplot(coefs, aes(x = variable, y = mean, ymin = low, ymax = high)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_pointrange() +
    coord_flip()

  ## return plot or unerlying dataframe
  if (plot == T) cp else coefs

}
