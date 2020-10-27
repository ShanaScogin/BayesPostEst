#' BayesPostEst Overview
#'
#' This package currently has nine main functions that can be used to generate 
#' and plot postestimation quantities after estimating Bayesian regression models using MCMC. 
#' The package combines functions written originally for Johannes Karreth's workshop on 
#' Bayesian modeling at the ICPSR Summer program. Currently BayesPostEst focuses mostly on 
#' generalized linear regression models for binary outcomes (logistic and probit regression). 
#' The vignette for this package has a walk-through of each function in action. 
#' Please refer to that to get an overview of all the functions, or visit the 
#' documentation for a specific function of your choice. Johannes Karreth's website
#' (http://www.jkarreth.net) also has resources for getting started with Bayesian 
#' analysis, fitting models, and presenting results.
#'
#' @section Main Functions:
#' \itemize{
#' \item \code{mcmcAveProb()}
#' \item \code{mcmcObsProb()}
#' \item \code{mcmcFD()}
#' \item \code{mcmcMargEff()}
#' \item \code{mcmcRocPrc()}
#' \item \code{mcmcRocPrcGen()}
#' \item \code{mcmcTab()}
#' \item \code{mcmcReg()}
#' \item \code{plot.mcmcFD()}
#' }
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

#' @importFrom caTools trapz
NULL

#' @importFrom coda as.mcmc HPDinterval
NULL

#' @importFrom texreg createTexreg texreg htmlreg
NULL

#' @importFrom utils getFromNamespace
NULL

#' @importFrom ROCR prediction performance
NULL

#> NULL