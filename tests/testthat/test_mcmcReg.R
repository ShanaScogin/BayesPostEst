data("jags_logit")
data("runjags_interactive")
data("mcmcpack_linear")

test_that("Simple model runs with mcmcReg", {
  
  expect_silent(mcmcReg(jags_logit))
  
  expect_silent(mcmcReg(jags_logit, pars = c("b[1]", "b[3]")))
  
  expect_silent(mcmcReg(jags_logit, pars = c("b[1]", "b[3]"),
                        coefnames = c("beta 1", "beta 3")))
  
  expect_silent(mcmcReg(jags_logit, pars = c("b"), coefnames = paste("beta", 1:3),
                        regex = T))
  
  expect_silent(mcmcReg(jags_logit, pars = c("b"), regex = T))
  
  expect_silent(mcmcReg(jags_logit, pars = c("b\\[1\\]", "b\\[3\\]"), regex = T))

  expect_silent(object3 <- mcmcReg(jags_logit, pars = c("b"),
     custom.coef.names = c("Variable 1",
                           "Variable 2",
                           "Variable 3"), regex = T))
  
  expect_silent(mcmcReg(jags_logit, coefnames = c("Variable 1",
                                           "Variable 2",
                                           "Variable 3",
                                           "deviance")))
  
  expect_silent(mcmcReg(jags_logit, gof = 1234,
                        gofnames = "Fake jags_logit Statistic"))
  
  expect_silent(mcmcReg(jags_logit, format = "html", file = "table"))
  
  ## remove table.html to avoid non-standard file note on check
  if (file.exists("table.html")) file.remove("table.html")

})

test_that("mcmcReg works with hpdi", {
  
  expect_match(mcmcReg(jags_logit, pointest = "median", hpdi = T),
               "667.30; 675.03")
  
  expect_match(mcmcReg(jags_logit, pointest = "median", hpdi = T,
                       format = "html"), "667.30; 675.03")
  
})

test_that("mcmcReg works with custom coef names", {
  
  expect_match(mcmcReg(jags_logit, coefnames = c("beta 1","beta 2", "beta 3",
                                                 "deviance")),
               "beta 1")
  
  expect_error(mcmcReg(list(jags_logit, jags_logit),
                       coefnames = c("beta 1","beta 2",
                                     "beta 3", "deviance")),
               regexp = "number of models does not match number of custom coefficient vectors")
  
})

test_that("mcmcReg works with standard deviation", {
  
  expect_match(mcmcReg(jags_logit, sd = T), "\\$\\(0.16\\)\\$")
  expect_match(mcmcReg(jags_logit, sd = T, format = "html"), ">\\(0.16\\)<")
  
})

test_that("mcmcReg works with pr direction", {
  
  expect_match(mcmcReg(jags_logit, pr = T), "\\$0.89\\$")
  expect_match(mcmcReg(jags_logit, pr = T, format = "html"), ">0.89<")
  
})

test_that("mcmcReg works with filenames", {
  
  expect_silent(mcmcReg(jags_logit, sd = T, file = "tab.tex"))
  expect_silent(mcmcReg(jags_logit, sd = T, format = "html", file = "tab.html"))
  
})

test_that("mcmcReg works with runjags", {
  
  expect_match(mcmcReg(runjags_interactive), "-0.55; -0.24")
  
})

test_that("mcmcReg works with mcmc", {
  
  expect_match(mcmcReg(mcmcpack_linear), "3.99;  6.00")
  
})

test_that("mcmcReg works with bugs", {
  
  expect_match(mcmcReg(bugs_model), "0.33; 3.64")
  
})

test_that("mcmcReg fails with multiple oject types", {
  
  expect_error(mcmcReg(list(jags_logit, runjags_interactive)))
  expect_error(mcmcReg(list(jags_logit, mcmcpack_linear)))
  expect_error(mcmcReg(list(runjags_interactive, mcmcpack_linear)))
  
})