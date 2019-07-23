#' BayesPostEst
#'
#' @section BayesPostEst functions: This package currently has 
#' six main functions 
#' \itemize{
#' \item \code{mcmcAveProb()}
#' \item \code{mcmcObsProb()}
#' \item \code{mcmcFD()}
#' \item \code{mcmcFDplot()}
#' \item \code{mcmcRocPrc()}
#' \item \code{mcmcTab()}
#' }
#' These functions can be used to generate postestimation quantities or to plot after 
#' estimating Bayesian regression models. The package combines functions 
#' written originally for Johannes Karreth's workshop on Bayesian modeling at the 
#' ICPSR Summer program. For now, the package focuses mostly on generalized linear 
#' regression models for binary outcomes (logistic and probit regression). 
#' The vignette for this package has a walk-through of each function in action. 
#' Please refer to that to get an overview of all the functions, or visit the 
#' documentation for a specific function of your choice. Johannes Karreth's website
#' (http://www.jkarreth.net) also has resources for getting started with Bayesian 
#' analysis, fitting models, and presenting results.
#'
#' @docType package
#' @name BayesPostEst
NULL
#> NULL

#' @importFrom rlang .data
NULL

#' @importFrom stats median pnorm model.matrix quantile
#' sd variable.names plogis
NULL

#' @importFrom ggplot2 ggplot geom_rect xlab ylab geom_vline scale_x_continuous
#' geom_text geom_bar facet_wrap scale_x_discrete scale_y_continuous aes
NULL

#' @importFrom dplyr summarize group_by tibble
NULL

#' @importFrom tidyr gather
NULL

#' @importFrom ggridges stat_density_ridges
NULL

#' @importFrom ggmcmc ggs
NULL

#' @importFrom reshape2 melt
NULL

#' @importFrom ROCR prediction performance
NULL

#' @importFrom caTools trapz
NULL

#' @importFrom coda as.mcmc
NULL

#' @importFrom rstanarm posterior_linpred
NULL

#' @importFrom R2jags jags
NULL

#> NULL
