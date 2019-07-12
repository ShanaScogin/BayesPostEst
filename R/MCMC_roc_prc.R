# R function to generate ROC and precision-recall curves after fitting 
# a logit/probit regression model in Stan
# Johannes Karreth

# stan_object: rstan object of class "stanfit", including parameters and 
# 	predicted values for each observation, with the prefix "pred"
# model_frame: model frame used to fit the model
# model_name: name of model for plotting

# Output:
# a list with four elements
# area_under_roc: area under ROC curve (scalar)
# area_under_prc: area under precision-recall curve (scalar)
# prc_dat: data to plot precision-recall curve (data frame)
# roc_dat: data to plot ROC curve (data frame)

# Credit: Area under the curve functions written by Andreas Beger
# <https://ssrn.com/abstract=2765419>


MCMC_roc_prc <- function(stan_object, 
                         model_frame, 
                         model_name = "Model"){
  
  # Prepare MCMC output
  require("ggmcmc")
  mcmc_ggs <- ggs(stan_object, family = "pred")
  
  require("dplyr")
  median_pred <- summarize(group_by(mcmc_ggs, Parameter),
                           y_pred = median(value) #,
                           # y_pred_lower = quantile(value, probs = ci[1]),
                           # y_pred_upper = quantile(value, probs = ci[2])
  )
  
  # Observed y and x						 
  median_pred$y_obs <- model_frame[, 1]
  # median_pred$x_obs <- model_frame[, xvar]
  
  # Area under the curve functions from Andreas Beger
  # <https://ssrn.com/abstract=2765419>
  
  require("ROCR")
  require("caTools")
  
  auc_roc <- function(obs, pred) {
    pred <- prediction(pred, obs)
    auc  <- performance(pred, "auc")@y.values[[1]]
    return(auc)
  }
  
  auc_pr <- function(obs, pred) {
    xx.df <- prediction(pred, obs)
    perf  <- performance(xx.df, "prec", "rec")
    xy    <- data.frame(recall = perf@x.values[[1]], 
                        precision = perf@y.values[[1]])
    
    # take out division by 0 for lowest threshold
    xy <- subset(xy, !is.nan(xy$precision))
    
    res   <- trapz(xy$recall, xy$precision)
    res
  }
  
  area_under_roc <- data.frame(auc = auc_roc(obs = median_pred$y_obs, pred = median_pred$y_pred), Model = model_name)
  
  area_under_prc <- data.frame(auc = auc_pr(obs = median_pred$y_obs, pred = median_pred$y_pred), Model = model_name)
  
  prediction_obj <- prediction(predictions = median_pred$y_pred,
                               labels = median_pred$y_obs)
  
  prc_performance_obj <- performance(prediction.obj = prediction_obj,
                                     measure = "prec",
                                     x.measure = "rec")
  
  prc_dat <- data.frame(x = prc_performance_obj@x.values,
                        y = prc_performance_obj@y.values,
                        Model = model_name)
  names(prc_dat) <- c("x", "y", "Model")
  
  roc_performance_obj <- performance(prediction.obj = prediction_obj,
                                     measure = "tpr",
                                     x.measure = "fpr")
  
  roc_dat <- data.frame(x = roc_performance_obj@x.values,
                        y = roc_performance_obj@y.values,
                        Model = model_name)
  names(roc_dat) <- c("x", "y", "Model")
  
  # Results as a list
  results <- list()
  results$area_under_roc <- area_under_roc
  results$area_under_prc <- area_under_prc
  results$prc_dat <- prc_dat
  results$roc_dat <- roc_dat
  
  return(results)
}