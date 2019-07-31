#'This function generates ROC and precision-recall curves 
#'after fitting a Bayesian logit or probit model. 
#'@title ROC and Precision-Recall Curves using Bayesian MCMC estimates
#'@description This function generates ROC and Precision-Recall curves 
#'after fitting a Bayesian logit or probit regression.
#'@param modelmatrix model matrix, including intercept (if the intercept is among the
#'parameters estimated in the model). Create with model.matrix(formula, data).
#'Note: the order of columns in the model matrix must correspond to the order of columns 
#'in the matrix of posterior draws in the \code{mcmcout} argument. See the \code{mcmcout}
#'argument for more and Beger (2016) for background.
#'@param mcmcout posterior distributions of all logit coefficients, 
#'in matrix form. This can be created from rstan, MCMCpack, R2jags, etc. and transformed
#'into a matrix using the function as.mcmc() from the coda package for \code{jags} class
#'objects, as.matrix() from base R for \code{mcmc}, \code{mcmc.list}, \code{stanreg}, and 
#'\code{stanfit} class objects, and \code{object$sims.matrix} for \code{bugs} class objects.
#'Note: the order of columns in this matrix must correspond to the order of columns 
#'in the model matrix. One can do this by examining the posterior distribution matrix and sorting the 
#'variables in the order of this matrix when creating the model matrix. A useful function for sorting 
#'column names containing both characters and numbers as 
#'you create the matrix of posterior distributions is \code{mixedsort()} fom the gtools package.
#'@param modelframe model frame in matrix form. Can be created using 
#'as.matrix(model.frame(formula, data))
#'@param curves logical indicator of whether or not to return values to plot the ROC or Precision-Recall 
#'curves. If set to \code{FALSE} (default), results are returned as a list without the extra
#'values. 
#'@param link type of generalized linear model; a character vector set to \code{"logit"} (default) or \code{"probit"}.
#'@param fullsims logical indicator of whether full object (based on all MCMC draws 
#'rather than their average) will be returned. Default is \code{FALSE}. Note:  If \code{TRUE}
#'is chosen, the function takes notably longer to execute.
#'@references Beger, Andreas. 2016. “Precision-Recall Curves.” Available at SSRN: 
#'https://ssrn.com/Abstract=2765419. http://dx.doi.org/10.2139/ssrn.2765419.
#'@return This function returns a list with 4 elements:
#'\itemize{
#'\item area_under_roc: area under ROC curve (scalar)
#'\item area_under_prc: area under precision-recall curve (scalar)
#'\item prc_dat: data to plot precision-recall curve (data frame)
#'\item roc_dat: data to plot ROC curve (data frame)
#'}
#'@examples
#' \donttest{
#' # simulating data
#' set.seed(123456)
#' b0 <- 0.2 # true value for the intercept
#' b1 <- 0.5 # true value for first beta
#' b2 <- 0.7 # true value for second beta
#' n <- 500 # sample size
#' X1 <- runif(n, -1, 1)
#' X2 <- runif(n, -1, 1)
#' Z <- b0 + b1 * X1 + b2 * X2
#' pr <- 1 / (1 + exp(-Z)) # inv logit function
#' Y <- rbinom(n, 1, pr) 
#' data <- data.frame(cbind(X1, X2, Y))
#' 
#' ## fitting with stan
#' library("rstanarm")
#' m <- rstanarm::stan_glm(Y ~ X1 + X2,
#'                         data = data,
#'                         family = binomial(link = "logit"))
#' 
#' ## using mcmcRocPrc with median draws
#' fit_sum <- mcmcRocPrc(sims = m,
#'                       modelframe = model.frame(m),
#'                       fullsims = FALSE)
#' 
#' fit_sum$area_under_roc
#' plot(x = fit_sum$roc_dat$x, 
#'      y = fit_sum$roc_dat$y, 
#'      type = "l", 
#'      main = "ROC")
#' abline(a = 0, b = 1)
#' 
#' fit_sum$area_under_prc
#' plot(x = fit_sum$prc_dat$x, 
#'      y = fit_sum$prc_dat$y, 
#'      type = "l", 
#'      main = "PRC")
#' 
#' }
#'@export

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
  
  if(fullsims == FALSE){
    y_pred <- apply(X = pred_prob, MARGIN = 2, FUN = function(x) median(x))
    
    # Observed y and x
    pred_obs <- data.frame(y_pred = y_pred, y_obs = modelframe[, 1])             
        
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