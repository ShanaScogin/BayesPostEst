rm(list = ls())
library("BayesPostEst")
setwd("/Users/johanneskarreth/Documents/Dropbox/Uni/1 - Papers/BayesPostEst/Github/paper")

df <- carData::Cowles
df$female <- (as.numeric(df$sex) - 2) * (-1)
df$volunteer <- as.numeric(df$volunteer) - 1
df$extraversion <- (df$extraversion - mean(df$extraversion)) / (2 * sd(df$extraversion))
df$neuroticism <- (df$neuroticism - mean(df$neuroticism)) / (2 * sd(df$neuroticism))

library("rstanarm")
fit <- stan_glm(volunteer ~ female + neuroticism + extraversion, 
                data = df, family = binomial(link = "logit"),
                prior = normal(0, 3),
                prior_intercept = normal(0, 3),
                chains = 4, 
                iter = 2000,
                seed = 123)

mcmcTab(fit, Pr = TRUE)
mcmcTab(fit, pars = c("female", "neuroticism", "extraversion"), ROPE = c(-0.1, 0.1))

mcmcmat <- as.matrix(fit)
mm <- model.matrix(volunteer ~ female + neuroticism + extraversion,
                   data = df)

aveprob.female <- mcmcAveProb(modelmatrix = mm,
                              mcmcout = mcmcmat[, 1:ncol(mm)],
                              xcol = 2,
                              xrange = c(0, 1),
                              link = "logit",
                              ci = c(0.025, 0.975),
                              fullsims = TRUE)
head(aveprob.female)

# plot to ../aveprob_female.png
library("ggplot2")
library("ggridges")
p_aveprob.female <- ggplot(data = aveprob.female, 
       aes(y = factor(x), x = pp)) + 
  stat_density_ridges(quantile_lines = TRUE, 
                      quantiles = c(0.025, 0.5, 0.975), vline_color = "white") + 
  scale_y_discrete(labels = c("Male", "Female")) + 
  ylab("") + 
  xlab("Estimated probability of volunteering") + 
  labs(title = "Probability of volunteering by gender", subtitle = "based on average-case approach") + 
  ggridges::theme_ridges()
ggsave(p_aveprob.female, file = "aveprob_female.png", width = 5, height = 5)

aveprob.extra <- mcmcAveProb(modelmatrix = mm,
                             mcmcout = mcmcmat[, 1:ncol(mm)],
                             xcol = 4,
                             xrange = seq(min(df$extraversion), max(df$extraversion), length.out = 20),
                             link = "logit",
                             ci = c(0.025, 0.975),
                             fullsims = FALSE)
head(aveprob.extra)   

# plot to aveprob_extra.png
p_aveprob.extra <- ggplot(data = aveprob.extra, 
       aes(x = x, y = median_pp)) + 
  geom_ribbon(aes(ymin = lower_pp, ymax = upper_pp), fill = "gray") + 
  geom_line() + 
  xlab("Extraversion") + 
  ylab("Estimated probability of volunteering") + 
  ylim(0, 1) + 
  labs(title = "Probability of volunteering by extraversion", subtitle = "based on average-case approach") + 
  theme_classic()
ggsave(p_aveprob.extra, file = "aveprob_extra.png", width = 5, height = 5)

# plot to aveprob.png
ggsave("aveprob.png", gridExtra::arrangeGrob(p_aveprob.female, p_aveprob.extra, ncol = 2), width = 10, height = 5)

# plot to obsprob_female.png
obsprob.female <- mcmcObsProb(modelmatrix = mm,
                                   mcmcout = mcmcmat[, 1:ncol(mm)],
                                   xcol = 2,
                                   xrange = c(0, 1),
                                   link = "logit",
                                   ci = c(0.025, 0.975),
                                   fullsims = TRUE)
p_obsprob.female <- ggplot(data = obsprob.female, 
            aes(y = factor(x), x = pp)) + 
  stat_density_ridges(quantile_lines = TRUE, 
                      quantiles = c(0.025, 0.5, 0.975), vline_color = "white") + 
  scale_y_discrete(labels = c("Male", "Female")) + 
  ylab("") + 
  xlab("Estimated probability of volunteering") + 
  labs(title = "Probability of volunteering by gender", subtitle = "based on observed-case approach") + 
  ggridges::theme_ridges()
ggsave(p_obsprob.female, file = "obsprob_female.png", width = 5, height = 5)

# plot to obsprob_extra.png
obsprob.extra <- mcmcObsProb(modelmatrix = mm,
                                  mcmcout = mcmcmat[, 1:ncol(mm)],
                                  xcol = 4,
                                  xrange = seq(min(df$extraversion), max(df$extraversion), length.out = 20),
                                  link = "logit",
                                  ci = c(0.025, 0.975),
                                  fullsims = FALSE)
p_obsprob.extra <- ggplot(data = obsprob.extra, 
            aes(x = x, y = median_pp)) + 
  geom_ribbon(aes(ymin = lower_pp, ymax = upper_pp), fill = "gray") + 
  geom_line() + 
  xlab("Extraversion") + 
  ylab("Estimated probability of volunteering") + 
  ylim(0, 1) + 
  labs(title = "Probability of volunteering by extraversion", subtitle = "based on observed-case approach") + 
  theme_classic()
ggsave(p_obsprob.extra, file = "obsprob_extra.png", width = 5, height = 5)

# plot to obsprob.png
ggsave("obsprob.png", gridExtra::arrangeGrob(p_obsprob.female, p_obsprob.extra, ncol = 2), width = 10, height = 5)

fdsum <- mcmcFD(modelmatrix = mm,
                mcmcout = mcmcmat[, 1:ncol(mm)],
                link = "logit",
                ci = c(0.025, 0.975),
                fullsims = FALSE)
fdsum
fdfull <- mcmcFD(modelmatrix = mm,
                 mcmcout = mcmcmat[, 1:ncol(mm)],
                 link = "logit",
                 ci = c(0.025, 0.975),
                 fullsims = TRUE)
summary(fdfull)

# plot to fd_full.png
p <- mcmcFDplot(fdfull = fdfull, ROPE = c(-0.01, 0.01))
p <- p + 
  scale_y_discrete(labels = c("Extraversion\nfrom 25th to 75th percentile", "Female\nvs. male", "Neuroticism\nfrom 25th to 75th percentile")) + 
  labs(title = "Differences in the probability of volunteering", 
              subtitle = "associated with changes in explanatory variables") + 
  ggridges::theme_ridges()
ggsave(p, file = "fd_full.png", width = 8, height = 6)

mf <- model.frame(volunteer ~ female + neuroticism + extraversion, data = df)
fitstats <- mcmcRocPrc(modelmatrix = mm,
                       mcmcout = mcmcmat[, 1:ncol(mm)],
                       modelframe = model.frame(fit),
                       curves = TRUE,
                       fullsims = FALSE)
fitstats$area_under_roc
fitstats$area_under_prc
# plot to roc_and_pr_curves.png
p_roc <- ggplot(data = fitstats$roc_dat, aes(x = x, y = y)) +
  geom_line() + 
  geom_abline(intercept = 0, slope = 1, color = "gray") + 
  labs(title = "ROC curve") + 
  xlab("1 - Specificity") + 
  ylab("Sensitivity") + 
  theme_classic()
p_pr <- ggplot(data = fitstats$prc_dat, aes(x = x, y = y)) +
  geom_line() + 
  labs(title = "Precision-Recall curve") + 
  xlab("Recall") + 
  ylab("Precision") + 
  theme_classic()
ggsave("roc_and_pr_curves.png", gridExtra::arrangeGrob(p_roc, p_pr, ncol = 2), width = 10, height = 5)

fitstats.fullsims <- mcmcRocPrc(modelmatrix = mm,
                                modelframe = mf,
                                mcmcout = mcmcmat[, 1:ncol(mm)],
                                curves = FALSE,
                                link = "logit",
                                fullsims = TRUE)
# plot to auc_distribution.png
p_auroc <- ggplot(fitstats.fullsims, aes(x = area_under_roc)) +
  geom_density(fill = "gray") +
  labs(title = "Area under the ROC curve", subtitle = "Full posterior distribution") + 
  theme_classic()
p_aupr <- ggplot(fitstats.fullsims, aes(x = area_under_prc)) +
  geom_density(fill = "gray") +
  labs(title = "Area under the Precision-Recall curve", subtitle = "Full posterior distribution") + 
  theme_classic()
ggsave("auc_distribution.png", gridExtra::arrangeGrob(p_auroc, p_aupr, ncol = 2), width = 10, height = 5)
