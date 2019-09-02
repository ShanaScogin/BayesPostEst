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
    ##     if (isTRUE(curves)) {
    ##         pred_prob <- as.data.frame(pred_prob)
    ##         curve_data <- lapply(pred_prob, yy = yvec, FUN = function(x, 
    ##             yy) {
    ##             rocr_pred <- ROCR::prediction(predictions = x, labels = yy)
    ##             rocr_prc <- ROCR::performance(prediction.obj = rocr_pred, 
    ##                 measure = "prec", x.measure = "rec")
    ##             prc_data <- data.frame(x = rocr_prc@x.values[[1]], 
    ##                 y = rocr_prc@y.values[[1]])
    ##             rocr_roc <- ROCR::performance(prediction.obj = rocr_pred, 
    ##                 measure = "tpr", x.measure = "fpr")
    ##             roc_data <- data.frame(x = rocr_roc@x.values[[1]], 
    ##                 y = rocr_roc@y.values[[1]])
    ##             list(prc_dat = prc_data, roc_dat = roc_data)
    ##         })
    ##         prc_dat <- lapply(curve_data, `[[`, "prc_dat")
    ##         roc_dat <- lapply(curve_data, `[[`, "roc_dat")
    ##     }
    ##     if (isTRUE(curves)) {
    ##         v_auc_roc <- sapply(roc_dat, function(xy) {
    ##             caTools::trapz(xy$x, xy$y)
    ##         })
    ##         v_auc_pr <- sapply(prc_dat, function(xy) {
    ##             xy <- subset(xy, !is.nan(xy$y))
    ##             caTools::trapz(xy$x, xy$y)
    ##         })
    ##     }
    ##     else {
    ##         v_auc_roc <- apply(pred_prob, MARGIN = 2, function(x) auc_roc(obs = yvec, 
    ##             pred = x))
    ##         v_auc_pr <- apply(pred_prob, MARGIN = 2, function(x) auc_pr(obs = yvec, 
    ##             pred = x))
    ##     }
    ##     if (curves & fullsims) {
    ##         out <- list(area_under_roc = v_auc_roc, area_under_prc = v_auc_pr, 
    ##             prc_dat = prc_dat, roc_dat = roc_dat)
    ##     }
    ##     if (curves & !fullsims) {
    ##         out <- list(area_under_roc = v_auc_roc, area_under_prc = v_auc_pr, 
    ##             prc_dat = prc_dat[[1]], roc_dat = roc_dat[[1]])
    ##     }
    ##     if (!curves & !fullsims) {
    ##         out <- list(area_under_roc = v_auc_roc, area_under_prc = v_auc_pr)
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
    ##  f1() 93.95821 101.00755 129.1437 115.6837 163.7863 208.5383    50   a
    ##  f2() 86.85721  94.96569 123.8703 104.6500 158.9830 200.1702    50   a

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
    ##  expr      min        lq     mean   median       uq      max neval cld
    ##  f1() 91.79774 108.44734 173.8986 139.7944 242.0546 324.8251    50   b
    ##  f2() 81.56758  96.16492 140.8706 113.8619 181.0781 268.2639    50  a

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
    ##  expr       min       lq     mean   median       uq      max neval cld
    ##  f1() 10.453441 11.21028 11.74057 11.55878 12.03372 14.28089    10   a
    ##  f2()  9.863609 10.91968 11.11132 11.01028 11.36222 12.75854    10   a

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
    ##  expr       min        lq      mean    median       uq      max neval cld
    ##  f1() 21.238577 21.542162 22.141907 21.809533 23.07233 23.34445    10   b
    ##  f2()  9.393025  9.680966  9.873146  9.821037 10.14483 10.36802    10  a
