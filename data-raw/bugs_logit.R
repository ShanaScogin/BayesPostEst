# 
#  Generate an example BUGS fitted model object
#

data(LINE, package = "rjags")
LINE$recompile()

## fitting the model with jags
bugs_model <- rjags::coda.samples(LINE, c("alpha", "beta", "sigma"),
                                  n.iter = 1000)

bugs_model <- R2WinBUGS::as.bugs.array(sims.array = as.array(bugs_model))

usethis::use_data(bugs_model, overwrite = TRUE)
