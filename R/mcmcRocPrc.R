#' ROC and Precision-Recall Curves using Bayesian MCMC estimates
#' 
#' Generate ROC and Precision-Recall curves after fitting a Bayesian logit or 
#' probit regression using [R2jags::jags()]
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
#' @param ... Used by methods
#' @param x a `mcmcRocPrc()` object
#' 
#' @references Beger, Andreas. 2016. “Precision-Recall Curves.” Available at 
#'   SSRN: [http://dx.doi.org/10.2139/ssrn.2765419](http://dx.doi.org/10.2139/ssrn.2765419)
#' 
#' @return Returns a list with length 2 or 4, depending on the on the "curves" 
#'   and "fullsims" argument values:
#'   
#'   - "area_under_roc": `numeric(1)`
#'   - "area_under_prc": `numeric(1)`
#'   - "prc_dat": only if `curves = TRUE`; a list with length 1 if `fullsims = FALSE`, longer otherwise
#'   - "roc_dat": only if `curves = TRUE`; a list with length 1 if `fullsims = FALSE`, longer otherwise
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
#' fit_sum                     
#' plot(fit_sum)
#' @export
#' @md

mcmcRocPrc <- function(object, 
                       yname, 
                       xnames, 
                       curves, 
                       fullsims) {
  
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
  # if not using fullsims, summarize across columns
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
      prc_dat = prc_dat[1],
      roc_dat = roc_dat[1]
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
  structure(
    out,
    y_pos_rate = mean(yvec),
    class = "mcmcRocPrc"
  )
}


#' Compute ROC and PR curve points
#' 
#' Faster replacements for calculating ROC and PR curve data than with 
#' [ROCR::prediction()] and [ROCR::performance()]
#' 
#' @details Replacements to use instead of a combination of [ROCR::prediction()] 
#' and [ROCR::performance()] to calculate ROC and PR curves. These functions are
#' about 10 to 20 times faster when using [mcmcRocPrc()] with `curves = TRUE` 
#' and/or `fullsims = TRUE`. 
#' 
#' See this [issue on GH (ShanaScogin/BayesPostEst#25)](https://github.com/ShanaScogin/BayesPostEst/issues/25) for more general details.
#' 
#' And [here is a note](https://github.com/andybega/BayesPostEst/blob/f1da23b9db86461d4f9c671d9393265dd10578c5/tests/profile-mcmcRocPrc.md) with specific performance benchmarks, compared to the 
#' old approach relying on ROCR.
#' 
#' @keywords internal
#' @md
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

#' @rdname compute_roc
#' @aliases compute_pr
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


#' @rdname mcmcRocPrc
#' 
#' @export
print.mcmcRocPrc <- function(x, ...) {
  
  auc_roc <- x$area_under_roc
  auc_prc <- x$area_under_prc
  
  has_curves <- !is.null(x$roc_dat)
  has_sims   <- length(auc_roc) > 1
  
  if (!has_sims) {
    roc_msg <- sprintf("%.3f", round(auc_roc, 3))
    prc_msg <- sprintf("%.3f", round(auc_prc, 3))
  } else {
    roc_msg <- sprintf("%.3f [80%%: %.3f - %.3f]",
                       round(mean(auc_roc), 3), 
                       round(quantile(auc_roc, 0.1), 3), 
                       round(quantile(auc_roc, 0.9), 3))
    prc_msg <- sprintf("%.3f [80%%: %.3f - %.3f]",
                       round(mean(auc_prc), 3), 
                       round(quantile(auc_prc, 0.1), 3), 
                       round(quantile(auc_prc, 0.9), 3))
  }
  
  cat("mcmcRocPrc object\n")
  cat(sprintf("curves: %s; fullsims: %s\n", has_curves, has_sims))
  cat(sprintf("AUC-ROC: %s\n", roc_msg))
  cat(sprintf("AUC-PR:  %s\n", prc_msg))
  
  invisible(x)
}

#' @rdname mcmcRocPrc
#' 
#' @param n plot method: if `fullsims = TRUE`, how many sample curves to draw?
#' @param alpha plot method: alpha value for plotting sampled curves; between 0 and 1
#' 
#' @export
plot.mcmcRocPrc <- function(x, n = 40, alpha = .5, ...) {

  stopifnot(
    "Use mcmcRocPrc(..., curves = TRUE) to generate data for plots" = (!is.null(x$roc_dat)),
    "alpha must be between 0 and 1" = (alpha >= 0 & alpha <= 1),
    "n must be > 0" = (n > 0)
  )
  
  obj<- x
  fullsims <- length(obj$roc_dat) > 1
  
  if (!fullsims) {
    
    graphics::par(mfrow = c(1, 2))
    plot(obj$roc_dat[[1]], type = "s", xlab = "FPR", ylab = "TPR")
    graphics::abline(a = 0, b = 1, lty = 3, col = "gray50")
    
    prc_dat <- obj$prc_dat[[1]]
    # use first non-NaN y-value for y[1]
    prc_dat$y[1] <- prc_dat$y[2]
    plot(prc_dat, type = "l", xlab = "TPR", ylab = "Precision",
         ylim = c(0, 1))
    graphics::abline(a = attr(x, "y_pos_rate"), b = 0, lty = 3, col = "gray50")
    
  } else {
    
    graphics::par(mfrow = c(1, 2))
    
    roc_dat <- obj$roc_dat
    
    x <- lapply(roc_dat, `[[`, 1)
    x <- do.call(cbind, x)
    colnames(x) <- paste0("sim", 1:ncol(x))
    
    y <- lapply(roc_dat, `[[`, 2)
    y <- do.call(cbind, y)
    colnames(y) <- paste0("sim", 1:ncol(y))
    
    xavg <- rowMeans(x)
    yavg <- rowMeans(y)
    
    plot(xavg, yavg, type = "n", xlab = "FPR", ylab = "TPR")
    samples <- sample(1:ncol(x), n)
    for (i in samples) {
      graphics::lines(
        x[, i], y[, i], type = "s",
        col = grDevices::rgb(127, 127, 127, alpha = alpha*255, maxColorValue = 255)
      )
    }
    graphics::lines(xavg, yavg, type = "s")
    
    # PRC
    # The elements of prc_dat have different lengths, unlike roc_dat, so we
    # have to do the central curve differently.
    prc_dat <- obj$prc_dat

    x <- lapply(prc_dat, `[[`, 1)
    y <- lapply(prc_dat, `[[`, 2)
    
    # Instead of combining the list of curve coordinates from each sample into
    # two x and y matrices, we can first make a point cloud with all curve 
    # points from all samples, and then average the y values at all distinct
    # x coordinates. The x-axis plots recall (TPR), which will only have as 
    # many distinct values as there are positives in the data, so this does 
    # not lose any information about the x coordinates. 
    point_cloud <- data.frame(
      x = unlist(x),
      y = unlist(y)
    )
    point_cloud <- stats::aggregate(point_cloud[, "y", drop = FALSE], 
                                    # factor implicitly encodes distinct values only,
                                    # since they will get the same labels
                                    by = list(x = as.factor(point_cloud$x)), 
                                    FUN = mean)
    point_cloud$x <- as.numeric(as.character(point_cloud$x))
    xavg <- point_cloud$x
    yavg <- point_cloud$y

    plot(xavg, yavg, type = "n", xlab = "TPR", ylab = "Precision", ylim = c(0, 1))
    samples <- sample(1:length(prc_dat), n)
    for (i in samples) {
      graphics::lines(
        x[[i]], y[[i]], 
        col = grDevices::rgb(127, 127, 127, alpha = alpha*255, maxColorValue = 255)
      )
    }
    graphics::lines(xavg, yavg)
    
  }
  
  invisible(x)
}

#' @rdname mcmcRocPrc
#' 
#' @param row.names see [base::as.data.frame()] 
#' @param optional see [base::as.data.frame()]
#' @param what which information to extract and convert to a data frame?
#' 
#' @export
as.data.frame.mcmcRocPrc <- function(x, row.names = NULL, optional = FALSE,
                                     what = c("auc", "roc", "prc"), ...) {
  what <- match.arg(what)
  if (what=="auc") {
    # all 4 output types have AUC, so this should work across the board
    return(as.data.frame(x[c("area_under_roc", "area_under_prc")]))
    
  } else if (what %in% c("roc", "prc")) {
    if (what=="roc") element <- "roc_dat" else element <- "prc_dat"
    
    # if curves was FALSE, there will be no curve data...
    if (is.null(x[[element]])) {
      stop("No curve data; use mcmcRegPrc(..., curves = TRUE)")
    }
    
    # Otherwise, there will be either one set of coordinates if mcmcmRegPrc()
    # was called with fullsims = FALSE, or else N_sims curve data sets.
    # If the latter, we can return a long data frame with an identifying 
    # "sim" column to delineate the sim sets. To ensure consistency in output,
    # also add this column when fullsims = FALSE.
    
    # averaged, single coordinate set
    if (length(x[[element]])==1L) {
      return(data.frame(sim = 1L, x[[element]][[1]]))
    }
    
    # full sims
    # add a unique ID to each coordinate set
    outlist <- x[[element]]
    outlist <- Map(cbind, sim = (1:length(outlist)), outlist)
    # combine into long data frame
    outdf <- do.call(rbind, outlist)
    return(outdf)
  } 
  stop("Developer error (I should not be here): please file an issue on GitHub")  # nocov
}

