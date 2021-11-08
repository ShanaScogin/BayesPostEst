# 
#  Generate an example JAGS probit fitted model
#

if (interactive()) {

data("sim_data")

## formatting the data for jags
datjags <- as.list(sim_data)
datjags$N <- length(datjags$Y)

## creating jags model
model <- function()  {
  
  for(i in 1:N){
    Y[i] ~ dbern(p[i])  ## Bernoulli distribution of y_i
    probit(p[i]) <- mu[i]    ## Update with probit link function
    
    mu[i] <- b[1] + 
      b[2] * X1[i] + 
      b[3] * X2[i]
  }
  
  for(j in 1:3){
    b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
  }
  
}

params <- c("b")
inits1 <- list("b" = rep(0, 3))
inits2 <- list("b" = rep(0, 3))
inits <- list(inits1, inits2)

## fitting the model with R2jags
set.seed(123)
fit <- R2jags::jags(data = datjags, inits = inits, 
                    parameters.to.save = params, n.chains = 2, n.iter = 2000, 
                    n.burnin = 1000, model.file = model)

jags_probit <- fit
usethis::use_data(jags_probit, overwrite = TRUE)

}
