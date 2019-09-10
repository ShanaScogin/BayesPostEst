#' ROC and Precision-Recall Curves using Bayesian MCMC estimates
#' 
#' Generate ROC and Precision-Recall curves after fitting a Bayesian logit or 
#' probit regression.
#' 
#' @param object A "rjags" object (see [R2jags::jags()]) for a fitted binary
#'   choice model.
#' @param yname (`character(1)`)\cr
#'   The name of the dependent variable, should match the variable name in the 
#'   JAGS data object.
#' @param xnames ([base::character()])\cr
#'   A character vector of the independent variable names, should match the 
#'   corresponding names in the JAGS data object.
#' @param curves logical indicator of whether or not to return values to plot 
#'   the ROC or Precision-Recall curves. If set to \code{FALSE} (default), 
#'   results are returned as a list without the extra values. 
#' @param fullsims logical indicator of whether full object (based on all MCMC
#'   draws rather than their average) will be returned. Default is `FALSE`. 
#'   Note: If `TRUE` is chosen, the function takes notably longer to execute.
#' 
#' @references Beger, Andreas. 2016. “Precision-Recall Curves.” Available at 
#'   SSRN: [http://dx.doi.org/10.2139/ssrn.2765419](http://dx.doi.org/10.2139/ssrn.2765419)
#' 
#' @return Returns a list; the specific structure depends on the combination
#'   of the "curves" and "fullsims" argument values. 
#'
#' @examples
#' # load simulated data and fitted model (see ?sim_data and ?jags_logit)
#' data("jags_logit")
#' 
#' # using mcmcRocPrc
#' fit_sum <- mcmcRocPrc(jags_logit,
#'                       yname = "Y",
#'                       xnames = c("X1", "X2"),
#'                       curves = TRUE,
#'                       fullsims = FALSE)
#' 
#' @export
#' @md
mcmcRocPrc <- function(object, yname, xnames, curves, fullsims) {
  
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
  
  pred_prob  <- as.data.frame(pred_prob)
  curve_data <- lapply(pred_prob, yy = yvec, FUN = function(x, yy) {
    prc_data <- compute_pr(yvec = yy, pvec = x)
    roc_data <- compute_roc(yvec = yy, pvec = x)
    list(
      prc_dat = prc_data,
      roc_dat = roc_data
    )
  })
  prc_dat <- lapply(curve_data, `[[`, "prc_dat")
  roc_dat <- lapply(curve_data, `[[`, "roc_dat")
  
  # Compute AUC-ROC values
  v_auc_roc <- sapply(roc_dat, function(xy) {
    caTools::trapz(xy$x, xy$y)
  })
  v_auc_pr  <- sapply(prc_dat, function(xy) {
    xy <- subset(xy, !is.nan(xy$y))
    caTools::trapz(xy$x, xy$y)
  })
  
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
      area_under_roc = v_auc_roc[[1]],
      area_under_prc = v_auc_pr[[1]]
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


compute_roc <- function(yvec, pvec) {
  porder <- order(pvec, decreasing = TRUE)
  yvecs  <- yvec[porder]
  pvecs  <- pvec[porder]
  p      <- sum(yvecs)
  n      <- length(yvecs) - p
  tp     <- cumsum(yvecs)
  tpr    <- tp/p
  fp     <- 1:length(yvecs) - tp
  fpr    <- fp/n
  
  dup_pred  <- rev(duplicated(pvecs))
  dup_stats <- duplicated(tpr) & duplicated(fpr)
  dups <- dup_pred | dup_stats
  
  fpr <- c(0, fpr[!dups])
  tpr <- c(0, tpr[!dups])
  
  roc_data <- data.frame(x = fpr,
                         y = tpr)
  roc_data
}

compute_pr <- function(yvec, pvec) {
  porder <- order(pvec, decreasing = TRUE)
  yvecs  <- yvec[porder]
  pvecs  <- pvec[porder]
  p      <- sum(yvecs)
  n      <- length(yvecs) - p
  tp     <- cumsum(yvecs)
  tpr    <- tp/p
  pp     <- 1:length(yvecs) 
  prec   <- tp/pp
  
  dup_pred  <- rev(duplicated(pvecs))
  dup_stats <- duplicated(tpr) & duplicated(prec)
  dups <- dup_pred | dup_stats
  
  prec <- c(NaN, prec[!dups])
  tpr <- c(0, tpr[!dups])
  
  prc_data <- data.frame(x = tpr,
                         y = prec)
  prc_data
}


# auc_roc and auc_pr are not really used, but keep around just in case
auc_roc <- function(obs, pred) {
  values <- compute_roc(obs, pred)
  caTools::trapz(values$x, values$y)
}

auc_pr <- function(obs, pred) {
  values <- compute_pr(obs, pred)
  caTools::trapz(values$x, values$y)
}




