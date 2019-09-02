mcmcRocPrc() speed
================

``` r
library("microbenchmark")
library("BayesPostEst")
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

Below is a 2nd version that works with less input (and no pre-processing
to extract data). It also uses `lapply` for the internal curve
computations.

``` r
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
```

``` r
data("sim_data")
data("jags_logit")
fit <- jags_logit
```

## curves = FALSE, fullsims = FALSE

``` r
f1 <- function() {
  mm <- model.matrix(Y ~ X1 + X2, data = sim_data)
  xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = sim_data))
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]
  mcmcRocPrc(modelmatrix = mm,
             modelframe = xframe,
             mcmcout = mcmc_mat,
             curves = FALSE,
             fullsims = FALSE)
}
f2 <- function() {
  mcmcRocPrc2(jags_logit, "Y", c("X1", "X2"), curves = FALSE, fullsims = FALSE)
}

microbenchmark(
  f1(),
  f2(),
  times = 50L
)
```

    ## Unit: milliseconds
    ##  expr      min       lq     mean   median       uq      max neval cld
    ##  f1() 92.50397 98.23218 126.6359 106.2386 162.1159 193.7190    50   a
    ##  f2() 87.42921 93.93193 122.3208 100.1355 157.5583 210.7985    50   a

## curves = TRUE, fullsims = FALSE

``` r
f1 <- function() {
  ## processing the data
  mm <- model.matrix(Y ~ X1 + X2, data = sim_data)
  xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = sim_data))
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]
  
  mcmcRocPrc(modelmatrix = mm,
             modelframe = xframe,
             mcmcout = mcmc_mat,
             curves = TRUE,
             fullsims = FALSE)
}

f2 <- function() {
  mcmcRocPrc2(jags_logit, "Y", c("X1", "X2"), curves = TRUE, fullsims = FALSE)
}

microbenchmark(
  f1(),
  f2(),
  times = 50L
)
```

    ## Unit: milliseconds
    ##  expr      min       lq     mean   median       uq      max neval cld
    ##  f1() 86.40022 106.7506 159.8728 120.3832 238.7542 271.4987    50   a
    ##  f2() 82.12333 100.7199 155.9313 117.7260 234.4008 268.7429    50   a

## curves = FALSE, fullsims = TRUE

``` r
f1 <- function() {
  ## processing the data
  mm <- model.matrix(Y ~ X1 + X2, data = sim_data)
  xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = sim_data))
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]
  mcmcRocPrc(modelmatrix = mm,
                   modelframe = xframe,
                   mcmcout = mcmc_mat,
                   curves = FALSE,
                   fullsims = TRUE)
}

f2 <- function() {
  mcmcRocPrc2(jags_logit, "Y", c("X1", "X2"), curves = FALSE, fullsims = TRUE)
}

microbenchmark(
  f1(),
  f2(),
  times = 10L
)
```

    ## Unit: seconds
    ##  expr      min       lq     mean   median       uq      max neval cld
    ##  f1() 11.06219 11.47752 12.14336 12.29647 12.72197 12.95722    10   b
    ##  f2() 10.42508 10.91673 11.39540 11.36660 11.80724 12.71077    10  a

## curves = TRUE, fullsims = TRUE

``` r
f1 <- function() {
  ## processing the data
  mm <- model.matrix(Y ~ X1 + X2, data = sim_data)
  xframe <- as.matrix(model.frame(Y ~ X1 + X2, data = sim_data))
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xframe)]
  mcmcRocPrc(modelmatrix = mm,
             modelframe = xframe,
             mcmcout = mcmc_mat,
             curves = TRUE,
             fullsims = TRUE)
}

f2 <- function() {
  mcmcRocPrc2(jags_logit, "Y", c("X1", "X2"), curves = TRUE, fullsims = TRUE)
}

microbenchmark(
  f1(),
  f2(),
  times = 10L
)
```

    ## Unit: seconds
    ##  expr      min       lq     mean   median       uq      max neval cld
    ##  f1() 20.85823 22.25647 23.29981 23.28911 24.65257 25.57629    10   b
    ##  f2() 19.14039 19.59245 20.36273 20.13533 20.83402 22.78937    10  a
