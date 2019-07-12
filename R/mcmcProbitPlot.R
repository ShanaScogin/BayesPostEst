#' An R function to calculate and plot predicted probabilities after a Bayesian probit model.
#'@title MCMCprobit.pp.plot
#'@description R function to calculate and plot predicted probabilities after a Bayesian probit model
#'@param model_matrix model matrix, including intercept, focal predictor in the second column
#'@param mcmc_out posterior distributions of all probit coefficients, 
#'in matrix form - can easily be created from rstan, MCMCpack, R2jags, etc.
#'@param x.col tbd
#'@param x.range tbd
#'@param xlabel tbd
#'@param ylabel tbd
#'@return output
#'@examples
#' \donttest{
#'   set.seed(123456)
#'   example for user goes here
#'   unit testing goes in testthat
#' }
#'@export
#'
MCMCprobit.pp.plot <- function(model.matrix, 
                               mcmc.out, 
                               x.col = 2, 
                               x.range, 
                               xlabel, 
                               ylabel){
  
  X <- matrix(rep(apply(X = model.matrix,
                        MARGIN = 2,
                        FUN = function(x) median(x)),
                  times = length(x.range)),
              nrow = length(x.range),
              byrow = TRUE)
  X[, x.col] <- x.range
  
  pp <- pnorm(t(X %*% t(mcmc.out)))
  
  colnames(pp) <- as.character(x.range)
  longFrame <- reshape2::melt(pp, id.vars = Var2)
  longFrame$Xvar <- as.character(longFrame$Var2)
  
  longSumframe <- dplyr::summarize(dplyr::group_by(longFrame, Xvar), 
                                   median.PP = median(value), 
                                   lower90 = quantile(value, probs = 0.05), 
                                   upper90 = quantile(value, probs = 0.95), 
                                   lower80 = quantile(value, probs = 0.1), 
                                   upper80 = quantile(value, probs = 0.9))
  
  pp.plot <- ggplot2::ggplot(data = longSumframe, aes(x = Xvar, y = median.PP))
  pp.plot <- pp.plot + geom_segment(data = longSumframe, aes(x = Xvar, xend = Xvar, y = lower90, yend = upper90), alpha = 0.35)
  # pp.plot <- pp.plot + geom_segment(data = longSumframe.m1, aes(x = IGOs, xend = IGOs, y = lower80, yend = upper80), alpha = 0.35, size = 2)
  pp.plot <- pp.plot + geom_point(size = 3)
  pp.plot <- pp.plot + scale_y_continuous(limits = c(0, NA))
  pp.plot <- pp.plot + theme_minimal() + xlab(xlabel) + ylab(ylabel)
  
  print(pp.plot)
}