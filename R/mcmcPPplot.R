## This function is not being included in the package
## at this time

##' An R function to calculate and plot predicted probabilities after a Bayesian logit or probit model.
##' @title Plot Predicted Probabilities after a Bayesian Logit or Probit Model
##' @description R function to calculate and plot predicted probabilities after a Bayesian probit model
##' @param model_matrix model matrix, including intercept, focal predictor in the second column
##' @param mcmc_out posterior distributions of all probit coefficients,
##' in matrix form - can easily be created from rstan, MCMCpack, R2jags, etc.
##' @param xcol tbd
##' @param xrange tbd
##' @param xlabel tbd
##' @param ylabel tbd
##' @return output
##' @examples
##' \donttest{
##'   set.seed(123456)
##' }

# mcmcPPplot <- function(model.matrix, 
#                        mcmc.out, 
#                        xcol = 2, 
#                        xrange, 
#                        xlabel, 
#                        ylabel){
#   
#   X <- matrix(rep(apply(X = model.matrix,
#                         MARGIN = 2,
#                         FUN = function(x) median(x)),
#                   times = length(xrange)),
#               nrow = length(xrange),
#               byrow = TRUE)
#   X[, xcol] <- xrange
#   
#   pp <- pnorm(t(X %*% t(mcmc.out)))
#   
#   colnames(pp) <- as.character(xrange)
#   longFrame <- reshape2::melt(pp, id.vars = Var2)
#   longFrame$Xvar <- as.character(longFrame$Var2)
#   
#   longSumframe <- dplyr::summarize(dplyr::group_by(longFrame, Xvar), 
#                                    median.PP = median(value), 
#                                    lower90 = quantile(value, probs = 0.05), 
#                                    upper90 = quantile(value, probs = 0.95), 
#                                    lower80 = quantile(value, probs = 0.1), 
#                                    upper80 = quantile(value, probs = 0.9))
#   
#   PPplot <- ggplot2::ggplot(data = longSumframe, aes(x = Xvar, y = median.PP))
#   PPplot <- PPplot + geom_segment(data = longSumframe, aes(x = Xvar, xend = Xvar, y = lower90, yend = upper90), alpha = 0.35)
#   # PPplot <- PPplot + geom_segment(data = longSumframe.m1, aes(x = IGOs, xend = IGOs, y = lower80, yend = upper80), alpha = 0.35, size = 2)
#   PPplot <- PPplot + geom_point(size = 3)
#   PPplot <- PPplot + scale_y_continuous(limits = c(0, NA))
#   PPplot <- PPplot + theme_minimal() + xlab(xlabel) + ylab(ylabel)
#   
#   print(PPplot)
# }