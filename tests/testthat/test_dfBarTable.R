test_that("Simple model runs with mcmcTab", {
  
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
data <- data.frame(cbind(X1, X2, Y))

# running function
plot <- dfBarTable(df = data)

})