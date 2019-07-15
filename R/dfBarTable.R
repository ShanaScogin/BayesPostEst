#'R function for summarizing a data frame as a table of bar plots
#'@title Generate bar plots for multiple variables
#'@description R function for summarizing a data frame as a table of bar plots
#'@param df dataframe
#'@param rows number of rows of plots within the figure
#'@return output
#'@examples
#' \donttest{
#'   set.seed(123456)
#' }
#'@export

dfBarTable <- function(df, rows = 2){


plot <- ggplot2::ggplot(data = tidyr::gather(df, factor_key = TRUE), 
                        aes(x = factor(value))) + 
  ggplot2::geom_bar() + 
  ggplot2::facet_wrap(~ key, scales = "free", as.table = TRUE, nrow = rows) + 
  ggplot2::scale_x_discrete(breaks = NULL) + 
  ggplot2::scale_y_continuous(breaks = NULL) + 
  ggplot2::xlab("") + 
  ggplot2::ylab("") 

return(plot)

}