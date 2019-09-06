# 
#  Generate an example JAGS probit fitted model
#

data("sim_data_linear")

## formatting the data for jags
datjags <- as.list(data)
datjags$N <- length(datjags$Y)

## creating jags model
model <- function()  {
  
  for(i in 1:N){
    Y[i] ~ dnorm(mu[i], sigma)  ## Bernoulli distribution of y_i
    
    mu[i] <- b[1] + 
      b[2] * X1[i] + 
      b[3] * X2[i] +
      b[4] * X1[i] * X2[i]
    
  }
  
  for(j in 1:4){
    b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
  }
  
  sigma ~ dexp(1)
  
}

params <- c("b")
inits1 <- list("b" = rep(0, 4))
inits2 <- list("b" = rep(0, 4))
inits <- list(inits1, inits2)

## fitting the model with R2jags
set.seed(123)
fit <- R2jags::jags(data = datjags, inits = inits, 
                    parameters.to.save = params, n.chains = 2, n.iter = 2000, 
                    n.burnin = 1000, model.file = model)

jags_interactive <- fit
usethis::use_data(jags_interactive, overwrite = TRUE)

