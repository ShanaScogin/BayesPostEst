## Note SRS 5/11/2022: Moved all the data from ~/data to
## test data - changing files to save to ~/testdata
## in the .rds format. Leaving previous code saving
## data with usethis::usedata() commented out for now

# 
#  Generate an example JAGS logit fitted model object
#

library(R2jags)

  #sim_data <- readRDS("~/BayesPostEst/tests/testdata/sim_data.rds")
  #data("sim_data")
  sim_data <- readRDS(file.path(TESTDATA_DIR, "sim_data.rds"))
  
  ## formatting the data for jags
  datjags <- as.list(sim_data)
  datjags$N <- length(datjags$Y)
  
  ## creating jags model
  model <- function()  {
    
    for(i in 1:N){
      Y[i] ~ dbern(p[i])  ## Bernoulli distribution of y_i
      logit(p[i]) <- mu[i]    ## Logit link function
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
  jags_logit <- R2jags::jags(data = datjags, inits = inits,
                      parameters.to.save = params, n.chains = 2, n.iter = 2000,
                      n.burnin = 1000, model.file = model,
                      progress.bar = "none")
  
  #usethis::use_data(jags_logit, overwrite = TRUE)
  #saveRDS(jags_logit, "tests/testdata/jags_logit.rds")
  saveRDS(jags_logit, file.path(TESTDATA_DIR, "jags_logit.rds"))




