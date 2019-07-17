# test_that("Simple model runs with mcmcAveProb", {
#   
#   library("rstanarm")
#   d <- carData::Cowles
#   d$female <- (as.numeric(d$sex) - 2) * (-1)
#   d$volunteer <- as.numeric(d$volunteer) - 1
#   
#   m <- stan_glm(volunteer ~ female + neuroticism + extraversion, 
#                 data = d,
#                 family = binomial(link = "logit"))
#   
#   # Using mcmcRocPrc
#   
#   # with median draws
#   
#   fit_sum <- mcmcRocPrc(sims = m,
#                         modelframe = model.frame(m),
#                         fullsims = FALSE)
#   
#   fit_sum$area_under_roc
#   plot(x = fit_sum$roc_dat$x, y = fit_sum$roc_dat$y, type = "l", main = "ROC")
#   abline(a = 0, b = 1)
#   
#   fit_sum$area_under_prc
#   plot(x = fit_sum$prc_dat$x, y = fit_sum$prc_dat$y, type = "l", main = "PRC")
#   
#   # full draws
#   
#   fit_full <- mcmcRocPrc(sims = m,
#                          modelframe = model.frame(m),
#                          fullsims = TRUE)
#   
#   # works but this is slooooow!
#   
#   # area under roc:
#   area_under_roc <- unlist(lapply(fit_full, '[[', 1))
#   area_under_prc <- unlist(lapply(fit_full, '[[', 2))
#   
# })
