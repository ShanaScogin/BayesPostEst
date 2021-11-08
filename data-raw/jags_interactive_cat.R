# 
#  Generate an example JAGS probit fitted model
#
if (interactive()) {

data("sim_data_interactive_cat")

## formatting the data for jags
datjags <- as.list(sim_data_interactive_cat)
datjags$N <- length(datjags$Y)

## creating jags model
model <- function()  {
  
  for(i in 1:N){
    Y[i] ~ dnorm(mu[i], sigma)  ## Bernoulli distribution of y_i
    
    mu[i] <- b[1] + 
      b[2] * X1[i] + 
      b[3] * X3[i] +
      b[4] * X1[i] * X3[i]
    
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
jags_interactive_cat <- R2jags::jags(data = datjags, inits = inits,
                                     parameters.to.save = params, n.chains = 2,
                                     n.iter = 2000, n.burnin = 1000,
                                     model.file = model)

usethis::use_data(jags_interactive_cat, overwrite = TRUE)

}
