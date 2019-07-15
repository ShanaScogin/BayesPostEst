#'An R function to plot first differences after a Bayesian logit or probit model.
#'@title Plot First Differences from MCMC output 
#'@description R function to plot first differences generated from MCMC output
#'@param fdfull Output generated from mcmcFD(..., full_sims = TRUE)
#'@param ROPE numeric vector of length two, defining the Region of Practical 
#'Equivalence around 0. Defaults to NULL
#'@return output
#'@examples
#' \donttest{
#'   set.seed(123456)
#' }
#' @export
#'
mcmcFDplot <- function(fdfull, 
                       ROPE = NULL){
  
  # convert fdfull to long data frame
  fd_dat <- tidyr::gather(as.data.frame(fdfull), 
                          key = variable, 
                          value = fd)
  
  # create first plot
  
  if(is.null(ROPE) == FALSE){
  fd_plot <- ggplot2::ggplot(data = fd_dat, aes(x = fd, y = variable)) + 
    ggplot2::geom_rect(xmin = ROPE[1], xmax = ROPE[2], ymin = 0, ymax = Inf, fill = "black") + 
    ggridges::stat_density_ridges(quantile_lines = TRUE, 
                                  quantiles = c(0.025, 0.5, 0.975),
                                  vline_color = "white") + 
    ggplot2::scale_x_continuous(labels = function(x) x * 100) + 
    ggplot2::xlab("Percentage point change in Pr(y = 1)\nas each predictor changes as indicated") + 
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
    geom_text(data = fd_annotate, aes(x = xpos, y = ypos, label = outROPE), 
                                 color = "black", nudge_y = 0.1, size = 4)
  }
  
  if(is.null(ROPE) == TRUE){
  fd_plot <- ggplot2::ggplot(data = fd_dat,
                               aes(x = fd, y = variable)) + 
    ggplot2::geom_vline(xintercept = 0) + 
    ggridges::stat_density_ridges(quantile_lines = TRUE, 
                                    quantiles = c(0.025, 0.5, 0.975),
                                    vline_color = "white") + 
    ggplot2::scale_x_continuous(labels = function(x) x*100) + 
    ggplot2::xlab("Percentage point change in Pr(y = 1)\nas each predictor changes as indicated") + 
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
      ggplot2::geom_text(data = fd_annotate, aes(x = xpos, y = ypos, label = out0), 
                color = "black", nudge_y = 0.1, size = 4)
    
  }
  
  # print plot
  print(fd_plot)
}