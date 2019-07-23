# BayesPostEst [![Build Status](https://travis-ci.com/ShanaScogin/BayesPostEst.svg?branch=master)](https://travis-ci.com/ShanaScogin/BayesPostEst)
An R package implementing functions to assist in Bayesian analysis.

# Introduction

BayesPostEst contains functions to generate postestimation quantities after estimating Bayesian regression models. The package combines functions written originally for [Johannes Karreth](http://www.jkarreth.net)'s workshop on Bayesian modeling at the [ICPSR Summer program](https://www.icpsr.umich.edu/icpsrweb/sumprog/). For now, the package focuses mostly on generalized linear regression models for binary outcomes (logistic and probit regression).

# Installation

Currently the package can be downloaded with the devtools package in R from GitHub. To do this, install devtools by calling:

```{r}
install.packages("devtools")
```

Now we can install from GitHub with the following line:

```{r}
devtools::install_github("ShanaScogin/BayesPostEst")
```

Once you have installed the package, you can access it by calling:

```{r}
library(BayesPostEst)
```
After the package is loaded, check out the `?BayesPostEst` to see a help file.


# General setup

Most functions in this package work with posterior distributions of parameters. These distributions need to be converted into a matrix. All functions in the package do this automatically for posterior draws generated by JAGS, BUGS, MCMCpack, rstan, and rstanarm. For posterior draws generated by other tools, users must convert these objects into a matrix, where rows represent iterations and columns represent parameters.

# Example data

This vignette uses the `Cowles` dataset ([Cowles and Davis 1987, British Journal of Social Psychology 26(2): 97-102](https://doi.org/10.1111/j.2044-8309.1987.tb00769.x)) from the [carData package](https://CRAN.R-project.org/package=carData). 

```{r}
df <- carData::Cowles
```

This data frame contains information on 1421 individuals in the following variables:

- neuroticism: scale from Eysenck personality inventory.
- extraversion: scale from Eysenck personality inventory.
- sex: a factor with levels: female; male.
- volunteer: volunteeing, a factor with levels: no; yes. This is the outcome variable for the running example in this vignette.

Before proceeding, we convert the two factor variables `sex` and `volunteer` into numeric variables. We also means-center and standardize the two continuous variables by dividing each by two standard deviations (Gelman and Hill 2007, Cambridge University Press).

```{r}
df$female <- (as.numeric(df$sex) - 2) * (-1)
df$volunteer <- as.numeric(df$volunteer) - 1
df$extraversion <- (df$extraversion - mean(df$extraversion)) / (2 * sd(df$extraversion))
df$neuroticism <- (df$neuroticism - mean(df$neuroticism)) / (2 * sd(df$neuroticism))
```

We estimate a Bayesian generalized linear model with the inverrse logit link function, where

$$
Pr(\text{Volunteering}_i) = \text{logit}^{-1}(\beta_1 + \beta_2 \text{Female}_i +  \beta_3 \text{Neuroticism}_i + \beta_4 \text{Extraversion}_i)
$$

BayesPostEst functions accommodate GLM estimates for both logit and probit link functions. The examples proceed with the logit link function. If we had estimated a probit regression, the corresponding argument `link` in relevant function calls would need to be set to `link = "probit"`. Otherwise, it is set to `link = "logit"` by default.

# Model estimation

To use BayesPostEst, we first estimate a Bayesian regression model. This vignette demonstrates four tools for doing so: JAGS (via the [`R2jags` package](https://cran.r-project.org/web/packages/R2jags/index.html)), [`MCMCpack`](https://cran.r-project.org/web/packages/MCMCpack/index.html), and the two Stan interfaces [`rstan`](https://cran.r-project.org/web/packages/rstan/index.html) and [`rstanarm`](https://cran.r-project.org/web/packages/rstanarm/index.html).

## JAGS

First, we prepare the data for JAGS ([Plummer 2017](http://sourceforge.net/projects/mcmc-jags/files/Manuals/4.x/})). Users need to combine all variables into a list and specify any other elements, like in this case N, the number of observations.

```{r}
dl <- as.list(df[, c("volunteer", "female", "neuroticism", "extraversion")])
dl$N <- nrow(df)
```

We then write the JAGS model into the working directory.

```{r}
mod.jags <- paste("	
model {
for (i in 1:N){
  volunteer[i] ~ dbern(p[i])  
  logit(p[i]) <- mu[i]   
  mu[i] <- b[1] + b[2] * female[i] + b[3] * neuroticism[i] + b[4] * extraversion[i]
  }

for(j in 1:4){
  b[j] ~ dnorm(0, 0.1)
  }

}
")
writeLines(mod.jags, "mod.jags")	
```

We then define the parameters for which we wish to retain posterior distributions and proivde starting values.

```{r}
params.jags <- c("b")
inits1.jags <- list("b" = rep(0, 4))
inits.jags <- list(inits1.jags, inits1.jags, inits1.jags, inits1.jags)
```

Now, fit the model.

```{r}
library("R2jags")
set.seed(123)

fit.jags <- jags(data = dl, inits = inits.jags, 
  parameters.to.save = params.jags, n.chains = 4, n.iter = 2000, 
  n.burnin = 1000, model.file = "mod.jags")
```

## MCMCpack

We estimate the same model using [MCMCpack](https://cran.r-project.org/web/packages/MCMCpack/index.html) (Martin, Quinn, and Park 2011, Journal of Statistical Software 42(9): 1-22).

```{r}
library("MCMCpack")
fit.MCMCpack <- MCMClogit(volunteer ~ female + neuroticism + extraversion, 
                          data = df, burning = 1000, mcmc = 2000, seed = 123,
                          b0 = 0, B0 = 0.1)
```

## RStan

We write the same model in Stan language.

```{r}
mod.stan <- paste("	
data {
  int<lower=0> N;
  int<lower=0,upper=1> volunteer[N];
  vector[N] female;
  vector[N] neuroticism;
  vector[N] extraversion;
}
parameters {
  vector[4] b;
}
model {
  volunteer ~ bernoulli_logit(b[1] + b[2] * female + b[3] * neuroticism + b[4] * extraversion);
  for(i in 1:4){
    b[i] ~ normal(0, 3); 
  }
}

")
writeLines(mod.stan, "mod.stan")	
```

We then load [rstan](https://cran.r-project.org/web/packages/rstan/index.html)...

```{r}
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

... and estimate the model, re-using the data in list format created for JAGS earlier.

```{r}
fit.stan <- stan(file = "mod.stan",  
           data = dl,         
           pars = c("b"),     
           chains = 4,        
           iter = 2000,       
           seed = 123)        
```

## rstanarm

Lastly, we use the [rstanarm](https://cran.r-project.org/web/packages/rstanarm/index.html) interface (Goodrich, Gabry, Ali, and Brilleman 2019) to estimate the same model again.

```{r}
library("rstanarm")
fit.rstanarm <- stan_glm(volunteer ~ female + neuroticism + extraversion, 
                          data = df, family = binomial(link = "logit"),
                         prior = normal(0, 3),
                         prior_intercept = normal(0, 3),
                         chains = 4, 
                         iter = 2000,
                         seed = 123)
```


# Tables of regression coefficients and other parameters

`mcmcTab` generates a table summarizing the posterior distributions of all parameters contained in the model object. This table can then be used to summarize parameter quantities. The function can directly process objects created by the following packages: R2jags, runjags, rjags, R2WinBUGS, MCMCpack, rstan, rstanarm. This includes the following object classes: `jags`, `rjags`, `bugs`, `mcmc`, `mcmc.list`, `stanreg`, `stanfit`. By default, `mcmcTab` generates a dataframe with one row per parameter and columns containing the median, standard deviation, and 95% credible interval of each parameter's posterior distribution.


```{r}
mcmcTab(fit.jags)
```

```{r}
mcmcTab(fit.MCMCpack)
```

```{r}
mcmcTab(fit.stan)
```

```{r}
mcmcTab(fit.rstanarm)
```

## Proportion of positive/negative draws

Users can add a column to the table that calculates the percent of posterior draws that have the same sign as the median of the posterior distribution.

```{r}
mcmcTab(fit.jags, Pr = TRUE)
```

## ROPE

Users can also define a "region of practical equivalence" (ROPE; [Kruschke 2013, Journal of Experimental Psychology 143(2): 573-603](http://dx.doi.org/10.1037/a0029146)). This region is a band of values around 0 that are "practically equivalent" to 0 or no effect. For this to be useful, all parameters (e.g. regression coefficients) must be on the same scale because mcmcTab accepts only one definition of ROPE for all parameters. Users can standardize regression coefficients to achieve this. Because we standardized variables earlier, the coefficients (except the intercept) are on a similar scale and we define the ROPE to be between -0.1 and 0.1.

```{r}
mcmcTab(fit.jags, pars = c("b[2]", "b[3]", "b[4]"), ROPE = c(-0.1, 0.1))
```

# Predicted probabilities

## `mcmcAveProb`

To evaluate the relationship between covariates and a binary outcome, this function calculates the predicted probability ($Pr(y = 1)$) at pre-defined values of one covariate of interest ($x$), while all other covariates are held at a "typical" value. This follows suggestions outlined in [King, Tomz, and Wittenberg (2000, American Journal of Political Science 44(2): 347-361)](https://www-jstor-org.proxy.library.nd.edu/stable/2669316) and elsewhere, which are commonly adopted by users of GLMs. The `mcmcAveProb` function by default calculates the median value of all covariates other than $x$ as "typical" values. 

Before moving on, we show how create a matrix of posterior draws of coefficients to pass onto these functions. Eventually, each function will contain code similar to the first section of `mcmcTab` to do this as part of the function.

```{r}
mcmcmat.jags <- as.matrix(coda::as.mcmc(fit.jags))

mcmcmat.MCMCpack <- as.matrix(fit.MCMCpack)
  
mcmcmat.stan <- as.matrix(fit.stan)
  
mcmcmat.rstanarm <- as.matrix(fit.rstanarm)
```

Next, we generate the model matrix to pass on to the function. A model matrix contains as many columns as estimated regression coefficients. The first column is a vector of 1s (corresponding to the intercept); the remaining columns are the observed values of covariates in the model. **Note: the order of columns in the model matrix must correspond to the order of columns in the matrix of posterior draws.**

```{r}
mm <- model.matrix(volunteer ~ female + neuroticism + extraversion,
                   data = df)
```

We can now generate predicted probabilities for different values of a covariate of interest.

### Sex

First, we generate full posterior distributions of the predicted probability of volunteering for a typical female and a typical male. In this function and `mcmcObsProb`, users specify the range of $x$ (here 0 and 1) as well as the number of the column of $x$ in the matrix of posterior draws as well as the model matrix. 

```{r}
aveprob.female.jags <- mcmcAveProb(modelmatrix = mm,
            mcmcout = mcmcmat.jags[, 1:ncol(mm)],
            xcol = 2,
            xrange = c(0, 1),
            link = "logit",
            ci = c(0.025, 0.975),
            fullsims = TRUE)
```

Users can then visualize this posterior distribution using the ggplot2 and ggridges packages.

```{r}
library("ggplot2")
library("ggridges")
ggplot(data = aveprob.female.jags, 
       aes(y = factor(x), x = pp)) + 
  stat_density_ridges(quantile_lines = TRUE, 
                quantiles = c(0.025, 0.5, 0.975), vline_color = "white") + 
  scale_y_discrete(labels = c("Male", "Female")) + 
  ylab("") + 
  xlab("Estimated probability of volunteering") + 
  labs(title = "Probability based on average-case approach")
```

### Extraversion

For continuous variables of interest, users may want to set `fullsims = FALSE` to obtain the median predicted probability along the range of $x$ as well as a lower and upper bound of choice (here, the 95% credible interval).

```{r}
aveprob.extra.jags <- mcmcAveProb(modelmatrix = mm,
            mcmcout = mcmcmat.jags[, 1:ncol(mm)],
            xcol = 4,
            xrange = seq(min(df$extraversion), max(df$extraversion), length.out = 20),
            link = "logit",
            ci = c(0.025, 0.975),
            fullsims = FALSE)
```

Users can then plot the resulting probabilities using any plotting functions, such as ggplot2.

```{r}
ggplot(data = aveprob.extra.jags, 
       aes(x = x, y = median_pp)) + 
  geom_ribbon(aes(ymin = lower_pp, ymax = upper_pp), fill = "gray") + 
  geom_line() + 
  xlab("Extraversion") + 
  ylab("Estimated probability of volunteering") + 
  ylim(0, 1) + 
  labs(title = "Probability based on average-case approach")
```

## `mcmcObsProb`

As an alternative to probabilities for "typical" cases, [Hanmer and Kalkan (2013, American Journal of Political Science 57(1): 263-277)](http://dx.doi.org/10.1111/j.1540-5907.2012.00602.x) suggest to calculate predicted probabilities for all observed cases and then derive an "average effect". In their words, the goal of this postestimation "is to obtain an estimate of the average effect in the population ... rather than seeking to understand the effect for the average case."

### Sex

We first calculate the average "effect" of sex on volunteering, again generating a full posterior distribution. Again, `xcol` represents the position of the covariate of interest, and `xrange` specifies the values for which $Pr(y = 1)$ is to be calculated.

```{r}
obsprob.female.jags <- mcmcObsProb(modelmatrix = mm,
            mcmcout = mcmcmat.jags[, 1:ncol(mm)],
            xcol = 2,
            xrange = c(0, 1),
            link = "logit",
            ci = c(0.025, 0.975),
            fullsims = TRUE)
```

Users can again plot the resulting densities.

```{r}
ggplot(data = obsprob.female.jags, 
       aes(y = factor(x), x = pp)) + 
  stat_density_ridges(quantile_lines = TRUE, 
                quantiles = c(0.025, 0.5, 0.975), vline_color = "white") + 
  scale_y_discrete(labels = c("Male", "Female")) + 
  ylab("") + 
  xlab("Estimated probability of volunteering") + 
  labs(title = "Probability based on observed-case approach")
```

### Extraversion

For this continuous predictor, we use `fullsims = FALSE`.

```{r}
obsprob.extra.jags <- mcmcObsProb(modelmatrix = mm,
            mcmcout = mcmcmat.jags[, 1:ncol(mm)],
            xcol = 4,
            xrange = seq(min(df$extraversion), max(df$extraversion), length.out = 20),
            link = "logit",
            ci = c(0.025, 0.975),
            fullsims = FALSE)
```

We then plot the resulting probabilities across observed cases.

```{r}
ggplot(data = obsprob.extra.jags, 
       aes(x = x, y = median_pp)) + 
  geom_ribbon(aes(ymin = lower_pp, ymax = upper_pp), fill = "gray") + 
  geom_line() + 
  xlab("Extraversion") + 
  ylab("Estimated probability of volunteering") + 
  ylim(0, 1) + 
  labs(title = "Probability based on observed-case approach")
```

# First differences

## `mcmcFD`

To summarize typical effects across covariates, we generate "first differences" (Long 1997, Sage Publications; [King, Tomz, and Wittenberg 2000, American Journal of Political Science 44(2): 347-361](https://www-jstor-org.proxy.library.nd.edu/stable/2669316)). This quantity represents, for each covariate, the difference in predicted probabilities for cases with low and high values of the respective covariate. For each of these differences, all other variables are held constant at their median.

```{r}
fdfull.jags <- mcmcFD(modelmatrix = mm,
                  mcmcout = mcmcmat.jags[, 1:ncol(mm)],
                  link = "logit",
                  ci = c(0.025, 0.975),
                  fullsims = TRUE)
summary(fdfull.jags)
```

The posterior distribution can be summarized as above, or users can directly obtain a summary when setting `fullsims` to FALSE.

```{r}
fdsum.jags <- mcmcFD(modelmatrix = mm,
                  mcmcout = mcmcmat.jags[, 1:ncol(mm)],
                  link = "logit",
                  ci = c(0.025, 0.975),
                  fullsims = FALSE)
fdsum.jags
```

Users can plot the median and credible intervals of the summary of the first differences.

```{r}
ggplot(data = fdsum.jags, 
       aes(x = median_fd, y = VarName)) + 
  geom_point() + 
  geom_segment(aes(x = lower_fd, xend = upper_fd, yend = VarName)) + 
  geom_vline(xintercept = 0) + 
  xlab("Change in Pr(Volunteering)") + 
  ylab("")
```

## `mcmcFDplot`

To make use of the full posterior distribution of first differences, we provide a dedicated plotting function. `mcmcFDplot` returns a ggplot2 object that can be further customized. The function is modeled after Figure 1 in [Karreth (2018, International Interactions 44(3): 463-490](http://dx.doi.org/10.1080/03050629.2018.1389728)). Users can specify a region of practical equivalence and print the percent of posterior draws to the right or left of the ROPE. If ROPE is not specified, the figure automatically prints the percent of posterior draws to the left or right of 0.

```{r}
mcmcFDplot(fdfull = fdfull.jags, ROPE = c(-0.01, 0.01))
```

The user can further customize the plot.

```{r}
p <- mcmcFDplot(fdfull = fdfull.jags, ROPE = c(-0.01, 0.01))
p + labs(title = "First differences") + ggridges::theme_ridges()
```

# Model fit

## `mcmcRocPrc`

One way to assess model fit is to calculate the area under the Receiver Operating Characteristic (ROC) and Precision-Recall curves. A short description of these curves and their utility for model assessment is provided in [Beger (2016)](http://dx.doi.org/10.2139/ssrn.2765419). The `mcmcRocPrc` function produces an object with four elements: the area under the ROC curve, the area under the PR curve, and two dataframes to plot each curve. When `fullsims` is set to `FALSE`, the elements represent the median of the posterior distribution of each quantity.

Because each of these measures relies on comparing the observed $y$ to $Pr(y = 1)$, the function requires both the posterior distribution of all regression coefficients as well as a model frame. This model frame contains all variables used to estimate the model, with the outcome variable in the first column and all other variables following thereafter.


```{r}
mf <- model.frame(volunteer ~ female + neuroticism + extraversion, data = df)
fitstats <- mcmcRocPrc(modelmatrix = mm,
                       modelframe = mf,
                       mcmcout = mcmcmat.jags[, 1:ncol(mm)],
                       curves = TRUE,
                       link = "logit",
                       fullsims = FALSE)
```

Users can then print the area under the each curve:

```{r}
fitstats$area_under_roc
```

```{r}
fitstats$area_under_prc
```

Users can also plot the ROC curve... 

```{r}
ggplot(data = fitstats$roc_dat, aes(x = x, y = y)) +
  geom_line() + 
  geom_abline(intercept = 0, slope = 1, color = "gray") + 
  labs(title = "ROC curve") + 
  xlab("1 - Specificity") + 
  ylab("Sensitivity")
```

... as well as the precision-recall curve.

```{r}
ggplot(data = fitstats$prc_dat, aes(x = x, y = y)) +
  geom_line() + 
  labs(title = "Precision-Recall curve") + 
  xlab("Recall") + 
  ylab("Precision")
```

To plot the posterior distribution of the area under the curves, users set the `fullsims` argument to `TRUE`. Unless a user wishes to plot credible intervals around the ROC and PR curves themselves, we recommend keeping `curves` at `FALSE` to avoid long computation time. 

```{r}
fitstats.fullsims <- mcmcRocPrc(modelmatrix = mm,
                                modelframe = mf,
                                mcmcout = mcmcmat.jags[, 1:ncol(mm)],
                                curves = FALSE,
                                link = "logit",
                                fullsims = TRUE)
```

We can then plot the posterior density of the area under each curve.

```{r}
ggplot(fitstats.fullsims, aes(x = area_under_roc)) + 
  geom_density() + 
  labs(title = "Area under the ROC curve")
```

```{r}
ggplot(fitstats.fullsims, aes(x = area_under_prc)) + 
  geom_density() + 
  labs(title = "Area under the Precision-Recall curve")
```

# What's Happening
New functions and enhancements to current functions are currently in the works. Please check back for more. 

# Contact
Please contact sscogin@nd.edu if you encounter any bugs with the package or have any comments. Feel free to check out [Johannes Karreth's website](http://www.jkarreth.net/index.html) for more resources on Bayesian analysis. 

# References
Beger, Andreas. 2016. “Precision-Recall Curves.” Available at SSRN: Https://Ssrn.com/Abstract=2765419. http://dx.doi.org/10.2139/ssrn.2765419.

Cowles, Michael, and Caroline Davis. 1987. “The Subject Matter of Psychology: Volunteers.” British Journal of Social Psychology 26 (2): 97–102. https://doi.org/10.1111/j.2044-8309.1987.tb00769.x.

Fox, John, Sanford Weisberg, and Brad Price. 2018. CarData: Companion to Applied Regression Data Sets. https://CRAN.R-project.org/package=carData.

Gelman, Andrew, and Jennifer Hill. 2007. Data Analysis Using Regression and Multilevel/Hierarchical Models. New York, NY: Cambridge University Press.

Goodrich, Ben, Jonah Gabry, Imad Ali, and Sam Brilleman. 2019. Rstanarm: Bayesian Applied Regression Modeling via Stan. http://mc-stan.org/.

Hanmer, Michael J., and Kerem Ozan Kalkan. 2013. “Behind the Curve: Clarifying the Best Approach to Calculating Predicted Probabilities and Marginal Effects from Limited Dependent Variable Models.” American Journal of Political Science 57 (1): 263–77. https://doi.org/10.1111/j.1540-5907.2012.00602.x.

Karreth, Johannes. 2018. “The Economic Leverage of International Organizations in Interstate Disputes.” International Interactions 44 (3): 463–90. https://doi.org/10.1080/03050629.2018.1389728.

King, Gary, Michael Tomz, and Jason Wittenberg. 2000. “Making the Most of Statistical Analyses: Improving Interpretation and Presentation.” American Journal of Political Science 44 (2): 347–61. http://www.jstor.org/stable/2669316.

Kruschke, John K. 2013. “Bayesian Estimation Supersedes the T-Test.” Journal of Experimental Psychology: General 142 (2): 573–603. https://doi.org/10.1037/a0029146.

Long, J. Scott. 1997. Regression Models for Categorial and Limited Dependent Variables. Thousand Oaks: Sage Publications.

Martin, Andrew D., Kevin M. Quinn, and Jong Hee Park. 2011. “MCMCpack: Markov Chain Monte Carlo in R.” Journal of Statistical Software 42 (9): 22. http://www.jstatsoft.org/v42/i09/.

Plummer, Martyn. 2017. “JAGS Version 4.3.0 User Manual.” http://sourceforge.net/projects/mcmc-jags/files/Manuals/4.x/.

Stan Development Team. 2019. RStan: The R Interface to Stan. http://mc-stan.org/.

