#'This function generates ROC and precision-recall curves 
#'after fitting a logit/probit regression model in Stan
#'references: Area under the curve functions written by Andreas Beger
#'<https://ssrn.com/abstract=2765419>
#'
#'@title ROC and Precision-recall Curve
#'@description This function generates ROC and precision-recall curves 
#'after fitting a logit/probit regression model in Stan
#'references: Area under the curve functions written by Andreas Beger
#'<https://ssrn.com/abstract=2765419>
#'@param stan_object rstan object of class "stanfit", including parameters and 
#'predicted values for each observation, with the prefix "pred"
#'@param model_frame model frame used to fit the model
#'@param model_name name of model for plotting
#'@return A list with four elements: 
#'area_under_roc: area under ROC curve (scalar)
#'area_under_prc: area under precision-recall curve (scalar)
#'prc_dat: data to plot precision-recall curve (data frame)
#'roc_dat: data to plot ROC curve (data frame)
#'@examples
#' \donttest{
#'   set.seed(123456)
#'   example for user goes here
#'   unit testing goes in testthat
#' }
#'@export

mcmcRocPrc <- function(stan_object, # this needs to be generalized
                         model_frame, 
                         model_name = "Model"){
  
  # Prepare MCMC output
  mcmc_ggs <- ggmcmc::ggs(stan_object, family = "pred")
  

  median_pred <- dplyr::summarize(dplyr::group_by(mcmc_ggs, Parameter),
                           y_pred = median(value) #,
                           # y_pred_lower = quantile(value, probs = ci[1]),
                           # y_pred_upper = quantile(value, probs = ci[2])
  )
  
  # Observed y and x						 
  median_pred$y_obs <- model_frame[, 1]
  # median_pred$x_obs <- model_frame[, xvar]
  
  # For more on the area under the curve functions see Andreas Beger
  # <https://ssrn.com/abstract=2765419>
  
  auc_roc <- function(obs, pred) {
    pred <- ROCR::prediction(pred, obs)
    auc  <- ROCR::performance(pred, "auc")@y.values[[1]]
    return(auc)
  }
  
  auc_pr <- function(obs, pred) {
    xx.df <- ROCR::prediction(pred, obs)
    perf  <- ROCR::performance(xx.df, "prec", "rec")
    xy    <- data.frame(recall = perf@x.values[[1]], 
                        precision = perf@y.values[[1]])
    
    # take out division by 0 for lowest threshold
    xy <- subset(xy, !is.nan(xy$precision))
    
    res   <- caTools::trapz(xy$recall, xy$precision)
    res
  }
  
  area_under_roc <- data.frame(auc = auc_roc(obs = median_pred$y_obs, pred = median_pred$y_pred), Model = model_name)
  
  area_under_prc <- data.frame(auc = auc_pr(obs = median_pred$y_obs, pred = median_pred$y_pred), Model = model_name)
  
  prediction_obj <- ROCR::prediction(predictions = median_pred$y_pred,
                               labels = median_pred$y_obs)
  
  prc_performance_obj <- ROCR::performance(prediction.obj = prediction_obj,
                                     measure = "prec",
                                     x.measure = "rec")
  
  prc_dat <- data.frame(x = prc_performance_obj@x.values,
                        y = prc_performance_obj@y.values,
                        Model = model_name)
  names(prc_dat) <- c("x", "y", "Model")
  
  roc_performance_obj <- ROCR::performance(prediction.obj = prediction_obj,
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