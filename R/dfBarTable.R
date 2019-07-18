#'R function for summarizing a data frame as a table of bar plots
#'@title Generate bar plots for multiple variables
#'@description R function for summarizing a data frame as a table of bar plots. 
#'This function helps with visualizing data before analysis 
#'@param data dataframe to be visualized
#'@param rows number of rows of plots within the figure
#'@return output
#'@examples
#' \donttest{
#'   set.seed(123456)
#' }
#'@export

dfBarTable <- function(data, rows = 2){


plot <- ggplot2::ggplot(data = tidyr::gather(df, factor_key = TRUE), 
                        aes(x = factor(.data$value))) + 
  ggplot2::geom_bar() + 
  ggplot2::facet_wrap(~ key, scales = "free", 
                      as.table = TRUE, 
                      nrow = rows) + 
  ggplot2::scale_x_discrete(breaks = NULL) + 
  ggplot2::scale_y_continuous(breaks = NULL) + 
  ggplot2::xlab("") + 
  ggplot2::ylab("") 

return(plot)

}