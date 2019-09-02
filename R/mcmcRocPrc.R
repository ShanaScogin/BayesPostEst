#' ROC and Precision-Recall Curves using Bayesian MCMC estimates
#' 
#' Generate ROC and Precision-Recall curves after fitting a Bayesian logit or 
#' probit regression.
#' 
#' @param modelmatrix model matrix, including intercept (if the intercept is 
#'   among the parameters estimated in the model). Create with 
#'   `model.matrix(formula, data)`. Note: the order of columns in the model 
#'   matrix must correspond to the order of columns in the matrix of posterior 
#'   draws in the \code{mcmcout} argument. See the `mcmcout`` argument for 
#'   more and Beger (2016) for background.
#' @param mcmcout posterior distributions of all logit coefficients, in matrix 
#'   form. This can be created from rstan, MCMCpack, R2jags, etc. and 
#'   transformed into a matrix using the function [coda::as.mcmc()] from the 
#'   coda package for `jags`` class objects, as.matrix() from base R for \code{mcmc}, \code{mcmc.list}, \code{stanreg}, and 
#'\code{stanfit} class objects, and \code{object$sims.matrix} for \code{bugs} class objects.
#'Note: the order of columns in this matrix must correspond to the order of columns 
#'in the model matrix. One can do this by examining the posterior distribution matrix and sorting the 
#'variables in the order of this matrix when creating the model matrix. A useful function for sorting 
#'column names containing both characters and numbers as 
#'you create the matrix of posterior distributions is \code{mixedsort()} fom the gtools package.
#' @param modelframe model frame in matrix form. Can be created using 
#'   `as.matrix(model.frame(formula, data))`.
#' @param curves logical indicator of whether or not to return values to plot the ROC or Precision-Recall 
#'curves. If set to \code{FALSE} (default), results are returned as a list without the extra
#'values. 
#' @param link type of generalized linear model; a character vector set to 
#'   `"logit"` (default) or `"probit"`.
#' @param fullsims logical indicator of whether full object (based on all MCMC
#'   draws rather than their average) will be returned. Default is `FALSE`. 
#'   Note:  If `TRUE` is chosen, the function takes notably longer to execute.
#' 
#' @references Beger, Andreas. 2016. “Precision-Recall Curves.” Available at 
#'   SSRN: [http://dx.doi.org/10.2139/ssrn.2765419]
#' 
#' @return This function returns a list with 4 elements:
#'  - area_under_roc: area under ROC curve (scalar)
#'  - area_under_prc: area under precision-recall curve (scalar)
#'  - prc_dat: data to plot precision-recall curve (data frame)
#'  - roc_dat: data to plot ROC curve (data frame)
#'
#'@examples
#' \donttest{
#' # load simulated data and fitted model (see ?sim_data and ?jags_logit)
#' data("sim_data")
#' data("jags_logit")
#' 
#' # processing the data
#' mm <- model.matrix(Y ~ X1 + X2, data = sim_data)
#' xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = sim_data))
#' mcmc <- coda::as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]
#' 
#' # using mcmcRocPrc
#' fit_sum <- mcmcRocPrc(modelmatrix = mm,
#'                       modelframe = xframe,
#'                       mcmcout = mcmc_mat,
#'                       curves = TRUE,
#'                       fullsims = FALSE)
#' }
#' 
#' @export
#' @md
mcmcRocPrc <- function(modelmatrix,
                       mcmcout,
                       modelframe,
                       curves = FALSE,
                       link = "logit",
                       fullsims = FALSE){
  
  if(link == "logit") {
    pred_prob <- plogis(t(modelmatrix %*% t(mcmcout)))    
  } else if (link == "probit") {
    pred_prob <- pnorm(t(modelmatrix %*% t(mcmcout)))
  } else {
    stop("Please enter a valid link argument")
  }
  
  if(missing(modelmatrix) | missing(mcmcout) | missing(modelframe)) {
    "Please enter the required arguments"
  }
  
  
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
  
  if(fullsims == FALSE){
    y_pred <- apply(X = pred_prob, MARGIN = 2, FUN = function(x) median(x))
    
    # Observed y and x
    pred_obs <- data.frame(y_pred = y_pred, y_obs = modelframe[, 1])             
    
    area_under_roc <- auc_roc(obs = pred_obs$y_obs, pred = pred_obs$y_pred)
    
    area_under_prc <- auc_pr(obs = pred_obs$y_obs, pred = pred_obs$y_pred)
    
    if(curves == FALSE){
      # Results as a list
      results <- list()
      results$area_under_roc <- area_under_roc
      results$area_under_prc <- area_under_prc
      
      return(results)
    }
    
    if(curves == TRUE){
      
      prediction_obj <- ROCR::prediction(predictions = pred_obs$y_pred,
                                         labels = pred_obs$y_obs)
      
      prc_performance_obj <- ROCR::performance(prediction.obj = prediction_obj,
                                               measure = "prec",
                                               x.measure = "rec")
      
      prc_dat <- data.frame(x = prc_performance_obj@x.values,
                            y = prc_performance_obj@y.values)
      names(prc_dat) <- c("x", "y")
      
      roc_performance_obj <- ROCR::performance(prediction.obj = prediction_obj,
                                               measure = "tpr",
                                               x.measure = "fpr")
      
      roc_dat <- data.frame(x = roc_performance_obj@x.values,
                            y = roc_performance_obj@y.values)
      names(roc_dat) <- c("x", "y")
      
      # Results as a list
      results <- list()
      results$area_under_roc <- area_under_roc
      results$area_under_prc <- area_under_prc
      results$prc_dat <- prc_dat
      results$roc_dat <- roc_dat
      
      return(results)
    }
  }
  
  if(fullsims == TRUE){
    RocPrcOneDraw <- function(pred_prob_vector){
      # run this function over each row (iteration) of the pred_prob matrix
      
      # y_pred <- apply(X = pred_prob, MARGIN = 2, FUN = function(x) median(x))
      
      # Observed y and x
      pred_obs <- data.frame(y_pred = pred_prob_vector, y_obs = modelframe[, 1])						 
      
      area_under_roc <- auc_roc(obs = pred_obs$y_obs, pred = pred_obs$y_pred)
      
      area_under_prc <- auc_pr(obs = pred_obs$y_obs, pred = pred_obs$y_pred)
      
      if(curves == FALSE){
        # Results as a list
        one_result <- c(area_under_roc, area_under_prc)
        return(one_result)
      }
      
      if(curves == TRUE){
        prediction_obj <- ROCR::prediction(predictions = pred_obs$y_pred,
                                           labels = pred_obs$y_obs)
        
        prc_performance_obj <- ROCR::performance(prediction.obj = prediction_obj,
                                                 measure = "prec",
                                                 x.measure = "rec")
        
        prc_dat <- data.frame(x = prc_performance_obj@x.values,
                              y = prc_performance_obj@y.values)
        names(prc_dat) <- c("x", "y")
        
        roc_performance_obj <- ROCR::performance(prediction.obj = prediction_obj,
                                                 measure = "tpr",
                                                 x.measure = "fpr")
        
        roc_dat <- data.frame(x = roc_performance_obj@x.values,
                              y = roc_performance_obj@y.values)
        names(roc_dat) <- c("x", "y")
        
        # Results as a list
        one_result <- list()
        one_result$area_under_roc <- area_under_roc
        one_result$area_under_prc <- area_under_prc
        one_result$prc_dat <- prc_dat
        one_result$roc_dat <- roc_dat
        
        return(one_result)
      }
    }
    
    if(curves == FALSE){
      all_results <- matrix(nrow = nrow(pred_prob), ncol = 2)
      for(i in 1:nrow(pred_prob)){
        all_results[i, ] <- RocPrcOneDraw(pred_prob[i, ])
      }
      all_results <- as.data.frame(all_results)
      names(all_results) <- c("area_under_roc", "area_under_prc")
    }
    
    if(curves == TRUE){
      all_results <- list()
      for(i in 1:nrow(pred_prob)){
        all_results[[i]] <- RocPrcOneDraw(pred_prob[i, ])
      }
    }
    
    return(all_results)
  }

}




mcmcRocPrc2 <- function(object, yname, xnames, curves, fullsims) {
  
  link_logit  <- any(grepl("logit", object$model$model()))
  link_probit <- any(grepl("probit", object$model$model()))
  
  mdl_data <- object$model$data()
  stopifnot(all(xnames %in% names(mdl_data)))
  stopifnot(all(yname %in% names(mdl_data)))
  
  # add intercept by default, maybe revisit this
  xdata <- as.matrix(cbind(X0 = 1L, as.data.frame(mdl_data[xnames])))
  yvec <- mdl_data[[yname]]
  
  pardraws <- as.matrix(coda::as.mcmc(object))
  # this is not very robust, assumes pars are 'b[x]'
  # for both this and the intercept addition above, maybe a more robust solution
  # down the road would be to dig into the object$model$model() string
  betadraws <- pardraws[, c(sprintf("b[%s]", 1:ncol(xdata - 1)))]
  
  if(isTRUE(link_logit)) {
    pred_prob <- plogis(xdata %*% t(betadraws))  
  } else if (isTRUE(link_probit)) {
    pred_prob <- pnorm(xdata %*% t(betadraws))
  } else {
    stop("Could not identify model link function")
  }
  
  # pred_prob is a [N, iter] matrix, i.e. each column are preds from one 
  # set of posterior samples
  # if not using fullsims, summarize accross columns
  if (isFALSE(fullsims)) {
    
    pred_prob <- as.matrix(apply(pred_prob, MARGIN = 1, median))
    
  }
  
  # Compute AUC-ROC values
  v_auc_roc <- apply(pred_prob, MARGIN = 2, function(x) auc_roc(obs = yvec, pred = x))
  v_auc_pr  <- apply(pred_prob, MARGIN = 2, function(x) auc_pr(obs = yvec, pred = x))
  
  if (isTRUE(curves)) {
    
    pred_prob  <- as.data.frame(pred_prob)
    curve_data <- lapply(pred_prob, yy = yvec, FUN = function(x, yy) {
      rocr_pred <- ROCR::prediction(predictions = x, labels = yy)
      rocr_prc  <- ROCR::performance(prediction.obj = rocr_pred,
                                     measure = "prec",
                                     x.measure = "rec")
      prc_data <- data.frame(x = rocr_prc@x.values[[1]],
                             y = rocr_prc@y.values[[1]])
      rocr_roc  <- ROCR::performance(prediction.obj = rocr_pred,
                                     measure = "tpr",
                                     x.measure = "fpr")
      roc_data <- data.frame(x = rocr_roc@x.values[[1]],
                             y = rocr_roc@y.values[[1]])
      list(
        prc_dat = prc_data,
        roc_dat = roc_data
      )
    })
    prc_dat <- lapply(curve_data, `[[`, "prc_dat")
    roc_dat <- lapply(curve_data, `[[`, "roc_dat")
  }
  
  # Recreate original output formats
  if (curves & fullsims) {
    out <- list(
      area_under_roc = v_auc_roc,
      area_under_prc = v_auc_pr,
      prc_dat = prc_dat,
      roc_dat = roc_dat
    )
  }
  if (curves & !fullsims) {
    out <- list(
      area_under_roc = v_auc_roc,
      area_under_prc = v_auc_pr,
      prc_dat = prc_dat[[1]],
      roc_dat = roc_dat[[1]]
    )
  }
  if (!curves & !fullsims) {
    out <- list(
      area_under_roc = v_auc_roc,
      area_under_prc = v_auc_pr
    )
  }
  if (!curves & fullsims) {
    out <- data.frame(
      area_under_roc = v_auc_roc,
      area_under_prc = v_auc_pr
    )
  }
  out
}


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




