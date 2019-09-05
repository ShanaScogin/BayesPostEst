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
to extract data). For speed improvements it has:

  - uses a custom curve calculation
  - uses `lapply` for the internal curve computations
  - uses a custom AUC calculation

## Verify the ROC/PR curve calculations are identical to ROCR

### ROC

First, ROC. `f1()` uses ROCR to calculate the curve. `f2()` is a
homebrew implementation. It has less checks but provided the inputs are
non-missing, finite, numeric vectors, it should work correctly. It can
handle duplicated predictions, e.g. an all constant prediction.

``` r
f1 <- function(pvec, yvec) {
  rocr_pred <- ROCR::prediction(predictions = pvec, labels = yvec)
  rocr_roc  <- ROCR::performance(prediction.obj = rocr_pred,
                                 measure = "tpr",
                                 x.measure = "fpr")
  roc_data <- data.frame(x = rocr_roc@x.values[[1]],
                         y = rocr_roc@y.values[[1]])
  roc_data
}

f2 <- function(pvec, yvec) {
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
```

Simple example to show duplicated values are correctly handled:

``` r
set.seed(1234)
pvec <- sample(c(rep(.2, 5), rep(.5, 5), rep(.8, 5)), size = 15)
yvec <- rbinom(n = 15, size = 1, prob = pvec)

out1 <- f1(pvec, yvec)
out2 <- f2(pvec, yvec)
testthat::expect_identical(out1, out2)
out1
```

    ##       x         y
    ## 1 0.000 0.0000000
    ## 2 0.125 0.5714286
    ## 3 0.500 0.8571429
    ## 4 1.000 1.0000000

``` r
out2
```

    ##       x         y
    ## 1 0.000 0.0000000
    ## 2 0.125 0.5714286
    ## 3 0.500 0.8571429
    ## 4 1.000 1.0000000

This data is captured from inside `mcmcRocPrc2()` just prior to the
curve calculations when `fullsims = TRUE`. Use one column (MCMC sample)
for testing.

``` r
pred_prob <- readRDS("pred-prob.rds")
pvec <- pred_prob[, 1]
yvec <- jags_logit$model$data()$Y

microbenchmark::microbenchmark(
  f1(pvec, yvec),
  f2(pvec, yvec),
  times = 200
)
```

    ## Unit: microseconds
    ##            expr      min        lq     mean    median       uq      max
    ##  f1(pvec, yvec) 2542.544 2760.9825 3651.107 3234.8870 3966.595 9496.122
    ##  f2(pvec, yvec)  241.980  263.4415  366.836  312.4295  397.426 1236.010
    ##  neval cld
    ##    200   b
    ##    200  a

### Homebrew PR curve calculation.

``` r
f1 <- function(pvec, yvec) {
  rocr_pred <- ROCR::prediction(predictions = pvec, labels = yvec)
  rocr_prc  <- ROCR::performance(prediction.obj = rocr_pred,
                                 measure = "prec",
                                 x.measure = "rec")
  prc_data <- data.frame(x = rocr_prc@x.values[[1]],
                         y = rocr_prc@y.values[[1]])
  prc_data
}

f2 <- function(pvec, yvec) {
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
```

Simple example to show duplicated values are correctly handled:

``` r
set.seed(1234)
pvec <- sample(c(rep(.2, 5), rep(.5, 5), rep(.8, 5)), size = 15)
yvec <- rbinom(n = 15, size = 1, prob = pvec)

out1 <- f1(pvec, yvec)
out2 <- f2(pvec, yvec)
testthat::expect_identical(out1, out2)
out1
```

    ##           x         y
    ## 1 0.0000000       NaN
    ## 2 0.5714286 0.8000000
    ## 3 0.8571429 0.6000000
    ## 4 1.0000000 0.4666667

``` r
out2
```

    ##           x         y
    ## 1 0.0000000       NaN
    ## 2 0.5714286 0.8000000
    ## 3 0.8571429 0.6000000
    ## 4 1.0000000 0.4666667

Benchmark on full example.

``` r
pred_prob <- readRDS("pred-prob.rds")
pvec <- pred_prob[, 1]
yvec <- jags_logit$model$data()$Y

microbenchmark::microbenchmark(
  f1(pvec, yvec),
  f2(pvec, yvec),
  times = 200
)
```

    ## Unit: microseconds
    ##            expr      min        lq      mean    median        uq       max
    ##  f1(pvec, yvec) 2340.147 2690.9105 3737.4873 3052.9975 3996.9930 17462.926
    ##  f2(pvec, yvec)  222.451  253.1455  380.2105  299.5875  360.8485  4636.517
    ##  neval cld
    ##    200   b
    ##    200  a

## Source code for v2

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
    ##         prc_data <- compute_pr(yvec = yy, pvec = x)
    ##         roc_data <- compute_roc(yvec = yy, pvec = x)
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
    ##  expr      min       lq     mean   median       uq      max neval cld
    ##  f1() 90.52845 94.71293 128.9952 101.7767 119.1933 376.0458    50   a
    ##  f2() 80.82302 87.51398 124.9077  90.8229 170.7834 240.5487    50   a

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
    ##  expr      min        lq     mean    median       uq      max neval cld
    ##  f1() 95.27485 102.20693 133.9310 104.68096 116.1674 257.3876    50   a
    ##  f2() 80.37629  86.68496 120.7663  90.39335 102.6267 258.2799    50   a

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
    ##  expr       min        lq     mean    median        uq      max neval cld
    ##  f1() 11.280333 11.350524 11.50423 11.543556 11.629966 11.72832    10   b
    ##  f2()  1.501663  1.687164  1.69418  1.705164  1.728044  1.77488    10  a

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
    ##  expr       min        lq      mean    median        uq       max neval
    ##  f1() 22.428992 26.025205 27.193896 27.083906 27.977556 33.280823    10
    ##  f2()  1.597306  1.734178  1.903603  1.822698  1.971806  2.548052    10
    ##  cld
    ##    b
    ##   a
