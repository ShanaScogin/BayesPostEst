% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmcCoefPlot.R
\name{mcmcCoefPlot}
\alias{mcmcCoefPlot}
\title{Coefficient Plots for MCMC Output}
\usage{
mcmcCoefPlot(
  mod,
  pars = NULL,
  pointest = "mean",
  ci = 0.95,
  hpdi = FALSE,
  sort = FALSE,
  plot = TRUE,
  regex = FALSE
)
}
\arguments{
\item{mod}{Bayesian model object generated by R2jags, rjags, R2WinBUGS, R2OpenBUGS, 
MCMCpack, rstan, rstanarm, and brms.}

\item{pars}{a scalar or vector of the parameters you wish to include in the table.
By default, \code{mcmcCoefPlot} includes all parameters saved in a model object. If a
model has lots of samples and lots of saved parameters, not explicitly specifying
a limited number of parameters to include via \code{pars} may take a long time
or produce an unreadable plot. \code{pars} can either be a vector with the
specific parameters to be included in the table e.g. \code{pars = c("beta[1]",
"beta[2]", "beta[3]")}, or they can be partial names that will be matched using
regular expressions e.g. \code{pars = "beta"} if \code{regex = TRUE}. Both of
these will include \code{beta[1]}, \code{beta[2]}, and \code{beta[3]} in the
plot. If \code{pars} is left blank, \code{mcmcCoefPlot} will exclude auxiliary
parameters such as \code{deviance} from JAGS or \code{lp__} from Stan.}

\item{pointest}{a character indicating whether to use the mean or median for
point estimates in the table.}

\item{ci}{a scalar indicating the confidence level of the uncertainty intervals.}

\item{hpdi}{a logical indicating whether to use highest posterior density intervals
or equal tailed credible intervals to capture uncertainty; default \code{FALSE}.}

\item{sort}{logical indicating whether to sort the point estimates to produce
a caterpillar or dot plot; default \code{FALSE}.}

\item{plot}{logical indicating whether to return a \code{ggplot} object or the
underlying tidy DataFrame; default \code{TRUE}.}

\item{regex}{use regular expression matching with \code{pars}?}
}
\value{
a \code{ggplot} object or a tidy DataFrame.
}
\description{
Coefficient plots for MCMC output using \code{ggplot2}
}
\examples{
\dontshow{.old_wd <- setwd(tempdir())}
\donttest{
if (interactive()) {
## simulating data
set.seed(123456)
b0 <- 0.2 # true value for the intercept
b1 <- 0.5 # true value for first beta
b2 <- 0.7 # true value for second beta
n <- 500 # sample size
X1 <- runif(n, -1, 1)
X2 <- runif(n, -1, 1)
Z <- b0 + b1 * X1 + b2 * X2
pr <- 1 / (1 + exp(-Z)) # inv logit function
Y <- rbinom(n, 1, pr)
df <- data.frame(cbind(X1, X2, Y))

## formatting the data for jags
datjags <- as.list(df)
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
fit <- R2jags::jags(data = datjags, inits = inits,
                    parameters.to.save = params, n.chains = 2, n.iter = 2000,
                    n.burnin = 1000, model.file = model)

## generating coefficient plot with all non-auxiliary parameters
mcmcCoefPlot(fit)
}
}

\dontshow{setwd(.old_wd)}
}
\author{
Rob Williams, \email{jayrobwilliams@gmail.com}
}
