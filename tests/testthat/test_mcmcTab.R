
data("jags_logit")

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

test_that("check with different sims input types", {
  
  data("jags_logit")
  
  # rjags
  expect_error(
    mcmcTab(jags_logit),
    NA
  )
  
  # mcmc.list
  expect_error(
    mcmcTab(as.mcmc(jags_logit)),
    NA
  )
  
  # mcmc
  expect_error(
    mcmcTab(as.mcmc(jags_logit)[[1]]),
    NA
  )
  
  # stanreg
  
  # stanfit
  
  
})

test_that("pars subsetting works", {
  
  data("jags_logit")
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = "b", 
                    Pr = FALSE,
                    ROPE = NULL)
  expect_equal(
    object$Variable, 
    factor(c(sprintf("b[%s]", 1:3)))
  )
  
  # however, this doesn't work (i guess regex complains)
  # object <- mcmcTab(jags_logit,
  #                   ci = c(0.025, 0.975),
  #                   pars = "b[1]",
  #                   Pr = FALSE,
  #                   ROPE = NULL)
  # expect_equal(
  #   object$Variable, 
  #   factor("b[1]")
  # )
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = c("b[1]", "b[2]"), 
                    Pr = FALSE,
                    ROPE = NULL)
  
  expect_equal(
    object$Variable, 
    factor(c(sprintf("b[%s]", 1:2)))
  )
  
})

test_that("ROPE argument works", {
  
  # valid input
  expect_message(
    object <- mcmcTab(jags_logit, pars = "b", ROPE = c(0, 1)),
    "This table contains an estimate for parameter"
  )
  
  expect_equal(
    object$PrOutROPE,
    c(0, 0.002, 0.011)
  )
  
  # invalid input; adjust the test at some point
  # expect_error(
  #   object <- mcmcTab(jags_logit, ROPE = 0)
  # )
  
})