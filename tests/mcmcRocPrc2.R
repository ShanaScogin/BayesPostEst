
dontrun <- function() {
  #comment this  otherwise R check complains
  #library("profvis")
  library("BayesPostEst")
  data("jags_logit")
  
  profvis(
    BayesPostEst:::mcmcRocPrc2(jags_logit, "Y", c("X1", "X2"), curves = TRUE, fullsims = TRUE)
  )
  
  
  # Try to speed up ROC calculations
  
  pred_prob <- readRDS("tests/pred-prob.rds")
  pvec <- pred_prob[, 1]
  yvec <- jags_logit$model$data()$Y
  
  auc_roc(yvec, pvec)
  
  rocr_pred <- ROCR::prediction(predictions = pvec, labels = yvec)
  rocr_roc  <- ROCR::performance(prediction.obj = rocr_pred,
                                 measure = "tpr",
                                 x.measure = "fpr")
  roc_data <- data.frame(x = rocr_roc@x.values[[1]],
                         y = rocr_roc@y.values[[1]])
  
  head(cbind(roc_data, y = c(NA, yvec[order(pvec)]), p = c(NA, sort(pvec))), 20)
  
  set.seed(1234)
  pvec <- sample(c(rep(.2, 5), rep(.5, 5), rep(.8, 5)), size = 15)
  yvec <- rbinom(n = 15, size = 1, prob = pvec)
  
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
  
  # check manual
  tpr1 <- numeric()
  fpr1 <- numeric()
  for (t in c(unique(pvecs), -Inf)) {
    p_pred <- pvecs > t
    tp1 <- sum(p_pred==1 & yvecs==1)
    tpr1 <- c(tpr1, tp1 / p)
    fp1 <- sum(p_pred==1 & yvecs==0) 
    fpr1 <- c(fpr1, fp1 / n)
  }
  
  
  pred_prob <- readRDS("tests/pred-prob.rds")
  pvec <- pred_prob[, 1]
  yvec <- jags_logit$model$data()$Y
  
  

}

