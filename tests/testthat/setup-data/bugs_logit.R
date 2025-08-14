### Shana comment Jan 2021:
### I'm deleting the data object created by this code and adding the simulation code 
### conditionally to the test files since CRAN is wanting the tarball to be smaller
### for coming release. Perhaps we can compress files going forward to keep a central data source?

#
#  Generate an example BUGS fitted model object
#
# 
# data(LINE, package = "rjags")
# LINE$recompile()
# 
# ## fitting the model with jags
# bugs_model <- rjags::coda.samples(LINE, c("alpha", "beta", "sigma"),
#                                   n.iter = 1000)
# 
# bugs_model <- R2WinBUGS::as.bugs.array(sims.array = as.array(bugs_model))
# 
# usethis::use_data(bugs_model, overwrite = TRUE)
