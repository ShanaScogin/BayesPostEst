## Note SRS 5/11/2022: Moved all the data from ~/data to
## test data - changing files to save to ~/testdata
## in the .rds format. Leaving previous code saving
## data with usethis::usedata() commented out for now

#
#   Generate simulated data used in examples
#

## simulating binary model data
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
sim_data <- data.frame(cbind(X1, X2, Y))
#usethis::use_data(sim_data, overwrite = TRUE)
saveRDS(sim_data, file.path(TESTDATA_DIR, "sim_data.rds"))

## simulating interactive linear model data
b3 <- -0.3 # true value for second beta
Z_interactive <- b0 + b1 * X1 + b2 * X2 + b3 * (X1 * X2)
Y_interactive <- rnorm(n, Z_interactive, 1)
sim_data_interactive <- data.frame(cbind(X1, X2, Y = Y_interactive))
#usethis::use_data(sim_data_interactive, overwrite = TRUE)
saveRDS(sim_data_interactive, file.path(TESTDATA_DIR, "sim_data_interactive.rds"))

## simulating interactive linear model with categorical moderator data
X3 <- rbinom(n, 5, .23) # categorical X
Z_interactive_cat <- b0 + b1 * X1 + b2 * X3 + b3 * (X1 * X3)
Y_interactive_cat <- rnorm(n, Z_interactive_cat, 1)
sim_data_interactive_cat <- data.frame(cbind(X1, X3, Y = Y_interactive_cat))
#usethis::use_data(sim_data_interactive_cat, overwrite = TRUE)
saveRDS(sim_data_interactive_cat, file.path(TESTDATA_DIR, "sim_data_interactive_cat.rds"))
