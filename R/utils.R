
check_ROPE_argument <- function(x) {
  
  if (is.null(x)) return(invisible(x))
  
  valid <- is.numeric(x) && length(x)==2 && (x[2] >= x[1])
  if (!valid) {
    stop("Invalid ROPE argument; must be a length 2 numeric vector like 'c(0, 1)'")
  }
  
  invisible(x)
}
