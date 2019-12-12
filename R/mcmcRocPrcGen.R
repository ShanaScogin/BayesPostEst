#'This function generates ROC and precision-recall curves
#'after fitting a Bayesian logit or probit model. 
#'@title ROC and Precision-Recall Curves using Bayesian MCMC estimates generalized
#'@description This function generates ROC and Precision-Recall curves 
#'after fitting a Bayesian logit or probit regression. For fast calculation for 
#'from an "rjags" object use \code{\link{mcmcRocPrc}}
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
#'you create the matrix of posterior distributions is \code{mixedsort()} from the gtools package.
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
#'
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
#' # formatting the data for jags
#' datjags <- as.list(data)
#' datjags$N <- length(datjags$Y)
#' 
#' # creating jags model
#' model <- function()  {
#'   
#'   for(i in 1:N){
#'     Y[i] ~ dbern(p[i])  ## Bernoulli distribution of y_i
#'     logit(p[i]) <- mu[i]    ## Logit link function
#'     mu[i] <- b[1] + 
#'       b[2] * X1[i] + 
#'       b[3] * X2[i]
#'   }
#'   
#'   for(j in 1:3){
#'     b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
#'   }
#'   
#' }
#' 
#' params <- c("b")
#' inits1 <- list("b" = rep(0, 3))
#' inits2 <- list("b" = rep(0, 3))
#' inits <- list(inits1, inits2)
#' 
#' ## fitting the model with R2jags
#' set.seed(123)
#' fit <- R2jags::jags(data = datjags, inits = inits, 
#'                     parameters.to.save = params, n.chains = 2, n.iter = 2000, 
#'                     n.burnin = 1000, model.file = model)
#' 
#' # processing the data
#' mm <- model.matrix(Y ~ X1 + X2, data = data)
#' xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = data))
#' mcmc <- coda::as.mcmc(fit)
#' mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]
#' 
#' # using mcmcRocPrcGen
#' fit_sum <- mcmcRocPrcGen(modelmatrix = mm,
#'                       modelframe = xframe,
#'                       mcmcout = mcmc_mat,
#'                       curves = TRUE,
#'                       fullsims = FALSE)
#' }
#'@export

mcmcRocPrcGen <- function(modelmatrix,
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
      
      prediction_obj <- prediction(predictions = pred_obs$y_pred,
                                         labels = pred_obs$y_obs)
      
      prc_performance_obj <- performance(prediction.obj = prediction_obj,
                                               measure = "prec",
                                               x.measure = "rec")
      
      prc_dat <- data.frame(x = prc_performance_obj@x.values,
                            y = prc_performance_obj@y.values)
      names(prc_dat) <- c("x", "y")
      
      roc_performance_obj <- performance(prediction.obj = prediction_obj,
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
        prediction_obj <- prediction(predictions = pred_obs$y_pred,
                                           labels = pred_obs$y_obs)
        
        prc_performance_obj <- performance(prediction.obj = prediction_obj,
                                                 measure = "prec",
                                                 x.measure = "rec")
        
        prc_dat <- data.frame(x = prc_performance_obj@x.values,
                              y = prc_performance_obj@y.values)
        names(prc_dat) <- c("x", "y")
        
        roc_performance_obj <- performance(prediction.obj = prediction_obj,
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