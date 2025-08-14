### Maintainer comment Jan 2021:
### I'm deleting the data object created by this code and adding the simulation code 
### conditionally to the test files since CRAN is wanting the tarball to be smaller
### for coming release. Perhaps we can compress files going forward to keep a central data source?

# 
#  Generate an example runjags interactive fitted model
#
# 
# #data("sim_data_interactive")
# sim_data_interactive <- readRDS(file.path(TESTDATA_DIR, "sim_data_interactive.rds"))
# 
# 
# ## formatting the data for jags
# datalist <- list(X = model.matrix(~ X1 * X2, sim_data_interactive),
#                  Y = sim_data_interactive[, 3],
#                  N = nrow(sim_data_interactive))
# 
# ## creating jags model
# model <- "model { 
# for(i in 1 : N){ 
# 	Y[i] ~ dnorm(beta %*% X[i, ], tau);
# } 
# for(i in 1:4) {
#   beta[i] ~ dnorm(0, 0.001)
# }
# tau ~ dexp(1)
# }"
# 
# ## fitting the model with runjags
# runjags_interactive <- runjags::run.jags(model = model, monitor = c("beta", "tau"),
#                                 data = datalist, n.chains = 2, method = "rjags")
# 
# ## save model object
# usethis::use_data(runjags_interactive, overwrite = TRUE)
