data("jags_logit")
data("runjags_interactive")
data("mcmcpack_linear")
data("bugs_model")

test_that("Simple model runs with mcmcTab", {
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = NULL, 
                    Pr = FALSE,
                    ROPE = NULL)
  
  value <- object[2, 2]
  check_against <- c(0.527)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  
  # ## checking with Johannes' previous function
  # devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
  # mcmcTab(coda::as.mcmc(fit))

})

test_that("mcmcTab works with different input types", {
  
  # rjags
  expect_equal(mcmcTab(jags_logit)[1,3], 0.09)
  
  # mcmc.list
  expect_equal(mcmcTab(coda::as.mcmc(jags_logit))[2,3], 0.166)
  
  # mcmc
  expect_equal(mcmcTab(mcmcpack_linear)[2,3],0.485)
  
  # bugs
  expect_equal(mcmcTab(bugs_model)[1,2], 1.031)
  
  # stanreg
  
  # stanfit
  
  
})

test_that("pars subsetting works", {
  
  data("jags_logit")
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = "b", 
                    Pr = FALSE,
                    ROPE = NULL,
                    regex = TRUE)
  expect_equal(
    object$Variable, 
    factor(c(sprintf("b[%s]", 1:3)))
  )
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = c("b\\[1\\]", "b\\[2\\]"), 
                    Pr = FALSE,
                    ROPE = NULL,
                    regex = TRUE)
  
  expect_equal(
    object$Variable, 
    factor(c(sprintf("b[%s]", 1:2)))
  )
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = c("b[1]", "b[3]"), 
                    Pr = FALSE,
                    ROPE = NULL)
  
  expect_equal(
    object$Variable, 
    factor(c(sprintf("b[%s]", c(1, 3))))
  )
  
})

test_that("ROPE argument works", {
  
  # valid input
  expect_message(
    object <- mcmcTab(jags_logit, pars = "b", ROPE = c(0, 1), regex = TRUE),
    "This table contains an estimate for parameter"
  )
  
  expect_equal(
    object$PrOutROPE,
    c(0, 0.002, 0.011)
  )
  
  # invalid input; adjust the test at some point
  expect_error(
    object <- mcmcTab(jags_logit, ROPE = 0),
    "Invalid ROPE argument"
  )
  
})
