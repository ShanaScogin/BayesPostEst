# 
#  Generate an example runjags interactive fitted model
#

data("sim_data_interactive")

## formatting the data for jags
data <- list(X = model.matrix(~ X1 * X2, sim_data_interactive),
             Y = sim_data_interactive[, 3],
             N = nrow(sim_data_interactive))

## creating jags model
model <- "model { 
for(i in 1 : N){ 
	Y[i] ~ dnorm(beta %*% X[i, ], tau);
} 
for(i in 1:4) {
  beta[i] ~ dnorm(0, 0.001)
}
tau ~ dexp(1)
}"

## fitting the model with runjags
runjags_interactive <- run.jags(model = model, monitor = c("beta", "tau"),
                                data = data, n.chains = 2, method = "rjags")

## save model object
usethis::use_data(runjags_interactive, overwrite = TRUE)
