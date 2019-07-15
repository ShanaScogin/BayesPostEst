#'R function for summarizing a data frame as a table of bar plots
#'@title Generate bar plots for multiple variables
#'@description R function for summarizing a data frame as a table of bar plots
#'@param df dataframe
#'@param rows number of rows of plots within the figure
#'@return output
#'@examples
#' \donttest{
#'   set.seed(123456)
#'   example for user goes here
#'   unit testing goes in testthat
#' }
#'@export

dfBarTable <- function(df, rows = 2){


plot <- ggplot2::ggplot(data = tidyr::gather(df, factor_key = TRUE), aes(x = factor(value))) + 
  geom_bar() + 
  facet_wrap(~ key, scales = "free", as.table = TRUE, nrow = rows) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + 
  xlab("") + 
  ylab("") 

return(plot)

}