
dontrun <- function() {
  #comment this  otherwise R check complains
  #library("profvis")
  library("BayesPostEst")
  data("jags_logit")
  
  profvis(
    BayesPostEst:::mcmcRocPrc2(jags_logit, "Y", c("X1", "X2"), curves = TRUE, fullsims = TRUE)
  )
}

