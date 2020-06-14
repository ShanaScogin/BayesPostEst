#' Fitted JAGS logit model
#' 
#' A fitted JAGS logit model generated with [R2jags::jags()]. See the example 
#' code below for how it was created. Used in examples and for testing. 
#' 
#' @format A class "rjags" object created by [R2jags::jags()]
#' 
#' @examples 
#' \donttest{
#' data("sim_data")
#'   
#' ## formatting the data for jags
#' datjags <- as.list(data)
#' datjags$N <- length(datjags$Y)
#' 
#' ## creating jags model
#' model <- function()  {
#' 
#'   for(i in 1:N){
#'     Y[i] ~ dbern(p[i])  ## Bernoulli distribution of y_i
#'       logit(p[i]) <- mu[i]    ## Logit link function
#'       mu[i] <- b[1] +
#'         b[2] * X1[i] +
#'         b[3] * X2[i]
#'   }
#' 
#'   for(j in 1:3){
#'     b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
#'   }
#' 
#' }
#' 
#' params <- c("b")
#' inits1 <- list("b" = rep(0, 3))
#' inits2 <- list("b" = rep(0, 3))
#' inits <- list(inits1, inits2)
#' 
#' ## fitting the model with R2jags
#' set.seed(123)
#' jags_logit <- R2jags::jags(data = datjags, inits = inits,
#'                          parameters.to.save = params, n.chains = 2, 
#'                          n.iter = 2000, n.burnin = 1000, model.file = model)
#'
#' }
#' 
#' @docType data
"jags_logit"

#' Fitted JAGS probit model
#' 
#' A fitted JAGS probit model generated with [R2jags::jags()]. See the example 
#' code below for how it was created. Used in examples and for testing. 
#' 
#' @format A class "rjags" object created by [R2jags::jags()]
#' 
#' @examples 
#' \donttest{
#' data("sim_data")
#'   
#' ## formatting the data for jags
#' datjags <- as.list(data)
#' datjags$N <- length(datjags$Y)
#' 
#' ## creating jags model
#' model <- function()  {
#' 
#'   for(i in 1:N){
#'     Y[i] ~ dbern(p[i])  ## Bernoulli distribution of y_i
#'       probit(p[i]) <- mu[i]    ## Update with probit link function
#'       mu[i] <- b[1] +
#'         b[2] * X1[i] +
#'         b[3] * X2[i]
#'   }
#' 
#'   for(j in 1:3){
#'     b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
#'   }
#' 
#' }
#' 
#' params <- c("b")
#' inits1 <- list("b" = rep(0, 3))
#' inits2 <- list("b" = rep(0, 3))
#' inits <- list(inits1, inits2)
#' 
#' ## fitting the model with R2jags
#' set.seed(123)
#' jags_probit <- R2jags::jags(data = datjags, inits = inits,
#'                          parameters.to.save = params, n.chains = 2, 
#'                          n.iter = 2000, n.burnin = 1000, model.file = model)
#'
#' }
#' 
#' @docType data
"jags_probit"

#' Fitted JAGS interactive linear model
#' 
#' A fitted JAGS linear model with interaction term generated with
#' [R2jags::jags()]. See the example  code below for how it was created. Used
#' in examples and for testing. 
#' 
#' @format A class "rjags" object created by [R2jags::jags()]
#' 
#' @examples 
#' \donttest{
#' data("sim_data_interactive")
#' 
#' ## formatting the data for jags
#' datjags <- as.list(data)
#' datjags$N <- length(datjags$Y)
#' 
#' ## creating jags model
#' model <- function()  {
#'   
#'   for(i in 1:N){
#'     Y[i] ~ dnorm(mu[i], sigma)  ## Bernoulli distribution of y_i
#'     
#'     mu[i] <- b[1] + 
#'       b[2] * X1[i] + 
#'       b[3] * X2[i] +
#'       b[4] * X1[i] * X2[i]
#'     
#'   }
#'   
#'   for(j in 1:4){
#'     b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
#'   }
#'   
#'   sigma ~ dexp(1)
#'   
#' }
#' 
#' params <- c("b")
#' inits1 <- list("b" = rep(0, 4))
#' inits2 <- list("b" = rep(0, 4))
#' inits <- list(inits1, inits2)
#' 
#' ## fitting the model with R2jags
#' set.seed(123)
#' jags_interactive <- R2jags::jags(data = datjags, inits = inits, 
#'                                  parameters.to.save = params, n.chains = 2,
#'                                  n.iter = 2000, n.burnin = 1000,
#'                                  model.file = model)
#'                                  
#' }
#' @docType data
"jags_interactive"

#' Fitted runjags interactive linear model
#' 
#' A fitted JAGS linear model with interaction term generated with
#' [runjags::run.jags()]. See the example  code below for how it was created. Used
#' in examples and for testing. 
#' 
#' @format A class "runjags" object created by [runjags::run.jags()]
#' 
#' @examples 
#' \donttest{
#' data("sim_data_interactive")
#' 
#' ## formatting the data for jags
#' data <- list(X = model.matrix(~ X1 * X2, sim_data_interactive),
#'              Y = sim_data_interactive[, 3],
#'              N = nrow(sim_data_interactive))
#' 
#' ## creating jags model
#' model <- "model { 
#' for(i in 1 : N){ 
#' 	Y[i] ~ dnorm(beta %*% X[i, ], tau);
#' } 
#' for(i in 1:4) {
#'   beta[i] ~ dnorm(0, 0.001)
#' }
#' tau ~ dexp(1)
#' }"
#' 
#' ## fitting the model with runjags
#' runjags_interactive <- run.jags(model = model, monitor = c("beta", "tau"),
#'                                 data = data, n.chains = 2, method = "rjags")
#' }
#' @docType data
"runjags_interactive"

#' Fitted JAGS interactive linear model with categorical moderator
#' 
#' A fitted JAGS linear model with interaction term generated with
#' [R2jags::jags()]. See the example  code below for how it was created. Used
#' in examples and for testing. 
#' 
#' @format A class "rjags" object created by [R2jags::jags()]
#' 
#' @examples 
#' \donttest{
#' data("sim_data_interactive_cat")
#' 
#' ## formatting the data for jags
#' datjags <- as.list(data)
#' datjags$N <- length(datjags$Y)
#' 
#' ## creating jags model
#' model <- function()  {
#'   
#'   for(i in 1:N){
#'     Y[i] ~ dnorm(mu[i], sigma)  ## Bernoulli distribution of y_i
#'     
#'     mu[i] <- b[1] + 
#'       b[2] * X1[i] + 
#'       b[3] * X3[i] +
#'       b[4] * X1[i] * X3[i]
#'     
#'   }
#'   
#'   for(j in 1:4){
#'     b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
#'   }
#'   
#'   sigma ~ dexp(1)
#'   
#' }
#' 
#' params <- c("b")
#' inits1 <- list("b" = rep(0, 4))
#' inits2 <- list("b" = rep(0, 4))
#' inits <- list(inits1, inits2)
#' 
#' ## fitting the model with R2jags
#' set.seed(123)
#' jags_interactive_cat <- R2jags::jags(data = datjags, inits = inits,
#'                                      parameters.to.save = params, n.chains = 2,
#'                                      n.iter = 2000, n.burnin = 1000,
#'                                      model.file = model)
#'                                  
#' }
#' @docType data
"jags_interactive_cat"


#' Simulated data for examples
#' 
#' Simulated data to fit example models against
#' 
#' @format a data.frame
#' 
#' @examples 
#' ## simulating data
#' set.seed(123456)
#' b0 <- 0.2 # true value for the intercept
#' b1 <- 0.5 # true value for first beta
#' b2 <- 0.7 # true value for second beta
#' n <- 500 # sample size
#' X1 <- runif(n, -1, 1)
#' X2 <- runif(n, -1, 1)
#' Z <- b0 + b1 * X1 + b2 * X2
#' pr <- 1 / (1 + exp(-Z)) # inv logit function
#' Y <- rbinom(n, 1, pr) 
#' sim_data <- data.frame(cbind(X1, X2, Y))
"sim_data"

#' Simulated data for examples
#' 
#' Simulated data to fit example models against
#' 
#' @format a data.frame
#' 
#' @examples 
#' set.seed(123456)
#' b0 <- 0.2 # true value for the intercept
#' b1 <- 0.5 # true value for first beta
#' b2 <- 0.7 # true value for second beta
#' b3 <- -0.3 # true value for second beta
#' n <- 500 # sample size
#' X1 <- runif(n, -1, 1)
#' X2 <- runif(n, -1, 1)
#' Z_interactive <- b0 + b1 * X1 + b2 * X2 + b3 * (X1 * X2)
#' Y_interactive <- rnorm(n, Z_interactive, 1)
#' sim_data_interactive <- data.frame(cbind(X1, X2, Y = Y_interactive))
"sim_data_interactive"

#' Simulated data for examples
#' 
#' Simulated data to fit example models against
#' 
#' @format a data.frame
#' 
#' @examples 
#' set.seed(123456)
#' b0 <- 0.2 # true value for the intercept
#' b1 <- 0.5 # true value for first beta
#' b2 <- 0.7 # true value for second beta
#' b3 <- -0.3 # true value for second beta
#' n <- 500 # sample size
#' X1 <- runif(n, -1, 1)
#' X3 <- rbinom(n, 5, .23)
#' Z_interactive_cat <- b0 + b1 * X1 + b2 * X3 + b3 * (X1 * X3)
#' Y_interactive_cat <- rnorm(n, Z_interactive_cat, 1)
#' sim_data_interactive_cat <- data.frame(cbind(X1, X3, Y = Y_interactive_cat))
"sim_data_interactive_cat"
