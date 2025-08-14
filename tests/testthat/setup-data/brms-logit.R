#
#   Generate an example brms logit model for testing
#

library("brms")

#data("sim_data")
sim_data <- readRDS(file.path(TESTDATA_DIR, "sim_data.rds"))

brms_logit <- brm(Y ~ X1 + X2, data = sim_data, family = bernoulli("logit"),
                  chains = 2, iter = 2000, warmup = 1000)

# saveRDS(brms_logit, "tests/testdata/brms-logit.rds")
saveRDS(brms_logit, file.path(TESTDATA_DIR, "brms-logit.rds"))

