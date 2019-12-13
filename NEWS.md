# BayesPostEst 0.2.0
* Added functions `mcmcCoefPlot()` and `mcmcMargEff()`.
* Added corresponding website and static docs using pkgdown.
* Added function `mcmcRocPrcGen()`. This function takes generalized objects and has the same functionality as `mcmcRocPrc()`, which only works for jags objects currently. It should be noted that `mcmcRocPrcGen()` works much more slowly than `mcmcRocPrc()`. Future version aim to merge and improve upon these functions.

# BayesPostEst 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Improved `mcmcRocPrc()` speed by replacing imported ROCR functions with faster custom ROC and PR curve calculators. The test example with "fullsims = TRUE" should now take 2s or less to run instead of the previous 10-30s. (#25)
* Removed ROCR package from dependencies. 
* Changed the `mcmcRocPrc()` interface. Instead of pre-calculating several data matrices, the function now takes a fitted "rjags" object ([R2jags::jags()]) as input along with names of the dependent and independent variables that were used in the model. 

# BayesPostEst 0.0.1

* Initial CRAN release. 
