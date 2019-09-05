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
source("../R/mcmcRocPrc.R")
mcmcRocPrc2
```

    ## function (object, yname, xnames, curves, fullsims) 
    ## {
    ##     link_logit <- any(grepl("logit", object$model$model()))
    ##     link_probit <- any(grepl("probit", object$model$model()))
    ##     mdl_data <- object$model$data()
    ##     stopifnot(all(xnames %in% names(mdl_data)))
    ##     stopifnot(all(yname %in% names(mdl_data)))
    ##     xdata <- as.matrix(cbind(X0 = 1L, as.data.frame(mdl_data[xnames])))
    ##     yvec <- mdl_data[[yname]]
    ##     pardraws <- as.matrix(coda::as.mcmc(object))
    ##     betadraws <- pardraws[, c(sprintf("b[%s]", 1:ncol(xdata - 
    ##         1)))]
    ##     if (isTRUE(link_logit)) {
    ##         pred_prob <- plogis(xdata %*% t(betadraws))
    ##     }
    ##     else if (isTRUE(link_probit)) {
    ##         pred_prob <- pnorm(xdata %*% t(betadraws))
    ##     }
    ##     else {
    ##         stop("Could not identify model link function")
    ##     }
    ##     if (isFALSE(fullsims)) {
    ##         pred_prob <- as.matrix(apply(pred_prob, MARGIN = 1, median))
    ##     }
    ##     pred_prob <- as.data.frame(pred_prob)
    ##     curve_data <- lapply(pred_prob, yy = yvec, FUN = function(x, 
    ##         yy) {
    ##         rocr_pred <- ROCR::prediction(predictions = x, labels = yy)
    ##         rocr_prc <- ROCR::performance(prediction.obj = rocr_pred, 
    ##             measure = "prec", x.measure = "rec")
    ##         prc_data <- data.frame(x = rocr_prc@x.values[[1]], y = rocr_prc@y.values[[1]])
    ##         rocr_roc <- ROCR::performance(prediction.obj = rocr_pred, 
    ##             measure = "tpr", x.measure = "fpr")
    ##         roc_data <- data.frame(x = rocr_roc@x.values[[1]], y = rocr_roc@y.values[[1]])
    ##         list(prc_dat = prc_data, roc_dat = roc_data)
    ##     })
    ##     prc_dat <- lapply(curve_data, `[[`, "prc_dat")
    ##     roc_dat <- lapply(curve_data, `[[`, "roc_dat")
    ##     v_auc_roc <- sapply(roc_dat, function(xy) {
    ##         caTools::trapz(xy$x, xy$y)
    ##     })
    ##     v_auc_pr <- sapply(prc_dat, function(xy) {
    ##         xy <- subset(xy, !is.nan(xy$y))
    ##         caTools::trapz(xy$x, xy$y)
    ##     })
    ##     if (curves & fullsims) {
    ##         out <- list(area_under_roc = v_auc_roc, area_under_prc = v_auc_pr, 
    ##             prc_dat = prc_dat, roc_dat = roc_dat)
    ##     }
    ##     if (curves & !fullsims) {
    ##         out <- list(area_under_roc = v_auc_roc, area_under_prc = v_auc_pr, 
    ##             prc_dat = prc_dat[[1]], roc_dat = roc_dat[[1]])
    ##     }
    ##     if (!curves & !fullsims) {
    ##         out <- list(area_under_roc = v_auc_roc[[1]], area_under_prc = v_auc_pr[[1]])
    ##     }
    ##     if (!curves & fullsims) {
    ##         out <- data.frame(area_under_roc = v_auc_roc, area_under_prc = v_auc_pr)
    ##     }
    ##     out
    ## }

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
    ##  expr      min        lq     mean   median       uq      max neval cld
    ##  f1() 91.18153 100.42942 141.2742 149.4513 171.1633 279.7083    50   a
    ##  f2() 86.93011  93.65343 126.3213 101.2978 159.1457 271.2318    50   a

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
    ##  expr       min       lq     mean   median       uq      max neval cld
    ##  f1() 103.58151 118.3493 204.7809 164.4192 259.8980 424.7429    50   a
    ##  f2()  91.27746 104.9720 173.4229 135.5493 244.0321 464.5473    50   a

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
    ##  expr       min        lq     mean   median       uq      max neval cld
    ##  f1() 11.516062 11.618626 12.23036 12.04608 12.82930 13.19144    10   b
    ##  f2()  9.870121  9.992884 10.61025 10.15661 11.64885 12.35606    10  a

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
    ##  expr       min        lq     mean   median       uq      max neval cld
    ##  f1() 21.008057 22.306516 23.15792 22.76411 23.93977 25.72352    10   b
    ##  f2()  9.096411  9.169684 10.36366 10.38649 11.22912 11.77002    10  a
