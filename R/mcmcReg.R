#' Regression Tables for MCMC Output
#'
#' Regression tables for MCMC model output using `texreg`.
#'
#' @param mod a single MCMC model object, or a list of model objects of the same class.
#' @param pars a scalar or vector of the parameters you wish to include in the table.
#' `stanfit` objects can use either the individual parameter names, or the
#' names of the indexed parameter to retrieve the entire parameter e.g.
#' `pars = 'beta'` will return `beta[1]`, `beta[2]`, and `beta[3]` for a Stan
#' model with a three element beta parameter. Parameter arguments for all
#' other model object types must contain the entire set of parameters you
#' wish to include in the table e.g. `pars = c('beta[1]', 'beta[2]', 'beta[3]')`.
#' This is necessary when combining JAGS models with different parameters because
#' `coda` does not support parameter family extraction like Stan. When combining
#' models with different parameters in one table, this argument also accepts a
#' list the length of the number of models.
#' @param point.est a character indicating whether to use the mean or median for
#' point estimates in the table.
#' @param ci a scalar indicating the confidence level of the uncertainty intervals.
#' @param hpdi a logical indicating whether to use highest posterior density intervals
#' or equal tailed credible intervals to capture uncertainty.
#' @param brms.re a logical indicating whether to include random effect estimates
#' from `brms` models.
#' @param custom.coef.names an optional vector or list of vectors containing parameter
#' names for each model. If there are multiple models, the list must have the same
#' number of elements as there are models, and the vector of names in each list
#' element must match the number of parameters. If not supplied, the function
#' will use the parameter names in the model object(s). Note that this replaces
#' the standard `custom.coef.names` argument in `texreg` because there is no
#' `extract` method for MCMC model objects, and many MCMC model objects do not
#' have unique parameter names.
#' @param gof a named list of goodness of fit statistics, or a list of such lists.
#' @param custom.gof.names an optional vector or list of vectors containing
#' goodness of fit statistic names for each model. If there are multiple models,
#' the list must have the same number of elements as there are models,
#' and the vector of names in each list element must match the number of goodness
#' of fit statistics.
#' @param format a character indicating `latex` or `html` output.
#' @param file optional file name to write table to file instead of printing to
#' console.
#' @param ... optional arguments to `texreg`.
#'
#' @details If using `custom.coef.map` with more than one model, you should rename
#' the parameters in the model objects to ensure that different parameters with the
#' same subscript are not conflated by `texreg` e.g. `beta[1]` could represent age
#' in one model and income in another, and `texreg` would combine the two if you
#' do not rename `beta[1]` to more informative names in the model objects.
#'
#' If `mod` is a `brmsfit` object or list of `brmsfit` objects, note that the
#' default `brms` names for coefficients are `b_Intercept` and `b`, so both of
#' these should be included in `par` if you wish to include the intercept in the
#' table. `mcmcreg()` will automatically look for random effects parameters which
#' `brms` labels with a leading `r_`. If you wish to include additional parameters
#' from more complex `brms` models, you can identify them by inspecting
#' `brmsfit$fit@model_pars`.
#'
#' @importFrom brms brm
#'
#' @return A formatted regression table in LaTeX or HTML format.
#' @export
#'
#' @author Rob Williams, \email{jayrobwilliams@@gmail.com}
#'
#' @examples
#' # simple linear model
#' fit1 <- brm(mpg ~ cyl + disp + hp, data = mtcars,
#'             family = gaussian())
#' mcmcreg(fit1, pars = c('b_Intercept', 'b'),
#' custom.coef.map = list('b_cyl' = 'Cylinders',
#'                        'b_disp' = 'Displacement',
#'                        'b_hp' = 'Horsepower',
#'                        'b_Intercept' = '(Constant)'))
#'
#' # random effects linear model
#' fit2 <- brm(mpg ~ cyl + disp + hp + (1 | gear),
#'             data = mtcars, family = gaussian())
#' mcmcreg(fit2, pars = c('b_Intercept', 'b'))
mcmcreg <- function(mod, pars, point.est = 'mean', ci = .95, hpdi = F,
                    brms.re = F, custom.coef.names = NULL, gof = numeric(0),
                    custom.gof.names = character(0),
                    format = 'latex', file, ...) {

  ## if only one model object, coerce to a list
  if (all(class(mod) != 'list')) mod <- list(mod)

  ## if only one custom coefficient names object, coerce to a list
  if (class(custom.coef.names) != 'list' & !is.null(custom.coef.names)) custom.coef.names <- list(custom.coef.names)

  ## if only one parameter vector, coerce to a list
  if (class(pars) != 'list') pars <- list(pars)

  ## if only one gof statistic scalar or vector, coerce to a list
  if (class(gof) != 'list') gof <- list(rep(gof, times = length(mod)))

  ## if only one gof statistic name scalar or vector, coerce to a list
  if (class(custom.gof.names) != 'list') custom.gof.names <- list(custom.gof.names)

  ## extract samples and variable names from stanfit object
  if (lapply(mod, inherits, 'stanfit')[[1]]) {

    ## extract coefficient names from list of model ojects
    coef_names <- mapply(function(x, y) rownames(rstan::summary(x, pars = y)$summary),
                         mod, pars, SIMPLIFY = F)

    ## extract posterior samples from list of model objects
    samps <- mapply(function(x, y) as.data.frame(rstan::extract(x, pars = y)),
                    mod, pars, SIMPLIFY = F)

  }

  ## extract samples and variable names from brmsfit object
  if (lapply(mod, inherits, 'brmsfit')[[1]]) {

    ## check for random effects parameters
    mod_ranefs <- lapply(mod, function(x) x$fit@model_pars[grep('r_', x$fit@model_pars)])

    ## concatenate random effects parameter names to pars
    if (brms.re) pars <- mapply(c, pars, mod_ranefs, SIMPLIFY = F)

    ## extract coefficient names from list of model ojects
    coef_names <- mapply(function(x, y) rownames(rstan::summary(x$fit, pars = y)$summary),
                         mod, pars, SIMPLIFY = F)

    ## extract posterior samples from list of model objects
    samps <- mapply(function(x, y) as.data.frame(rstan::extract(x$fit, pars = y)),
                    mod, pars, SIMPLIFY = F)

  }

  ## extract samples and variable names from runjags object
  if (lapply(mod, inherits, 'runjags')[[1]]) {

    ## extract posterior samples from list of model objects
    samps <- mapply(function(x, y) runjags:::as.mcmc.list.runjags(x, vars = y),
                    mod, pars, SIMPLIFY = F)

    ## average over chains and convert to dataframe
    samps <- lapply(samps, function(x) as.data.frame(Reduce("+", x) / length(x)))

    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)

  }

  ## extract samples and variable names from mcmc.list object
  if (lapply(mod, inherits, 'mcmc.list')[[1]]) {

    ## extract posterior samples from list of model objects
    samps <- lapply(mod, function(x) as.data.frame(Reduce("+", x) / length(x)))

    ## drop columns not in pars
    samps <- mapply(function(x, y) x[, colnames(x) %in% y], samps, pars,
                    SIMPLIFY = F)

    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)

  }

  ## extract samples and variable names from mcmc object
  if (lapply(mod, inherits, 'mcmc')[[1]]) {

    ## extract posterior samples from list of model objects
    samps <- mapply(function(x) coda:::as.data.frame.mcmc(x, vars = y),
                    mod, pars, SIMPLIFY = F)

    ## extract coefficient names from dataframe
    coef_names <- lapply(samps, colnames)

  }

  ## calculate point estimate of posterior density
  if (point.est == 'mean') {

    samps_pe <- lapply(samps, function(x) apply(x, 2, mean))

  } else {

    samps_pe <- lapply(samps, function(x) apply(x, 2, median))

  }

  ## calculate uncertainty interval for ci argument
  if (hpdi == F) {

    samps_ci <- lapply(samps, function(x) apply(x, 2, quantile,
                                                probs = c(.5 - ci/2, .5 + ci/2)))

  } else {

    samps_ci <- lapply(samps, function(x) t(coda::HPDinterval(coda::as.mcmc(x),
                                                            prob = ci)))

  }

  ## if coefficent names supplied, replace names from model object(s)
  if (!is.null(custom.coef.names) & !is.list(custom.coef.names)) coef_names <- list(custom.coef.names)
  if (!is.null(custom.coef.names)) coef_names <- custom.coef.names

  ##
  if (length(mod) != length(coef_names)) {

    stop('number of models does not match number of custom coefficient vectors')

  }

  ## create list of texreg object(s) with point estimates and interval
  tr_list <- mapply(function(v, w, x, y, z) texreg::createTexreg(coef.names = v,
                                                           coef = w,
                                                           ci.low = x[1, ],
                                                           ci.up = x[2, ],
                                                           gof = y,
                                                           gof.names = z),
                    coef_names, samps_pe, samps_ci, gof, custom.gof.names)

  ## create LaTeX output
  if (format == 'latex') {

    ## create LaTeX code
    tr <- texreg::texreg(l = tr_list, ...)

    ## replace confidence w/ credible or highest posterior density in texreg output
    if (hpdi == F) {

      tr <- sub('outside the confidence interval',
                paste('outside ', ci * 100 ,'\\\\% credible interval', sep = ''),
                tr)

    } else {

      tr <- sub('outside the confidence interval',
                paste('outside ', ci * 100 ,'\\\\% highest posterior density interval',
                      sep = ''), tr)

    }

    ## return LaTeX code to console or write to file
    if (missing(file)) {

      return(tr)

    } else {

      ## remove newline at start of LaTeX code
      tr <- sub('^\\n', '', tr)

      tex_file <- file(paste(file, 'tex', sep = '.'))
      writeLines(tr, tex_file, sep = '')
      close(tex_file)

    }

  }

  ## create HTML output
  if (format == 'html') {

    hr <- texreg::htmlreg(l = tr_list, ...)

    ## replace confidence w/ credible or highest posterior density in texreg output
    if (hpdi == F) {

      hr <- sub('outside the confidence interval',
                paste('outside ', ci * 100, '% credible interval', sep = ''),
                hr)

    } else {

      tr <- sub('outside the confidence interval',
                paste('outside ', ci * 100, '% highest posterior density interval',
                      sep = ''), hr)

    }

    ## return html code to console or write to file
    if (missing(file)) {

      return(hr)

    } else {

      hmtl_file <- file(paste(file, 'html', sep = '.'))
      writeLines(hr, html_file, sep = '')
      close(html_file)

    }

  }

}
