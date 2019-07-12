# R function to calculate first differences after a Bayesian probit model 
# Johannes Karreth

# model_matrix: model matrix, including intercept
# mcmc_out: posterior distributions of all probit coefficients, 
#  in matrix form - can easily be created from rstan, MCMCpack, R2jags, etc.

MCMCprobit.fd.mat <- function(model_matrix, mcmc_out, 
                              credint = c(0.05, 0.95), 
                              percentiles = c(0.25, 0.75), 
                              full_sims = FALSE){
  
  fd.mat <- matrix(NA, ncol = 3, nrow = ncol(model_matrix) - 1)
  colnames(fd.mat) <- c("Median", "Lower", "Upper")
  rownames(fd.mat) <- colnames(model_matrix)[-1]
  
  fd.full <- matrix(rep(NA),
                    ncol = ncol(model_matrix) - 1,
                    nrow = nrow(mcmc_out),
                    byrow = TRUE)
  colnames(fd.full) <- colnames(model_matrix)[-1]
  
  for (i in 2:ncol(model_matrix)){
    
    X <- matrix(rep(apply(X = model_matrix,
                          MARGIN = 2,
                          FUN = function(x) median(x)),
                    times = 2),
                nrow = 2,
                byrow = TRUE)
    X[, i] <- ifelse(length(unique(model_matrix[, i])) == 2 & range(model_matrix[, i]) == c(0, 1), c(0, 1), 
                     quantile(model_matrix[, i], probs = percentiles))
    
    # X[, i] <- quantile(model_matrix[, i], probs = percentiles)
    
    pp <- pnorm(t(X %*% t(mcmc_out)))
    
    fd <- pp[, 2] - pp[, 1]
    
    fd.mat[i-1, 1] <- quantile(fd, probs = c(0.5))
    fd.mat[i-1, 2] <- quantile(fd, probs = c(credint[1]))
    fd.mat[i-1, 3] <- quantile(fd, probs = c(credint[2]))
    
    fd.full[, i-1] <- fd
    
  }
  
  fd.dat <- as.data.frame(fd.mat)
  fd.dat$VarName <- rownames(fd.mat)
  fd.dat$VarID <- row(fd.mat)[, 1]
  
  if(full_sims == FALSE){
    return(fd.dat)
  } 
  
  if(full_sims == TRUE){
    return(fd.full)
  }
  
}