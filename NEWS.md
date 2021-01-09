# BayesPostEst 0.3.1

* Removed package dependency on JAGS. Since rjags and R2jags were in the package Imports previously, and they in turn depend on a JAGS installed, the package would not install without JAGS. Now, all specific MCMC implementations are in the package Suggests and the package can be installed in an agnostic way without the need for JAGS, Stan, or any other specific MCMC library. 
  + Conditional was added in the vignette for these packages (#77)
* There were several changes in mcmcRocPrc:
  + The function has now become a generic with S3 methods for different types of input, e.g. "rjags" objects created by [R2jags::jags()] or "stanfit" via [rstan::stan()]. The default method takes a matrix of predicted probabilities and vector of observed outcomes as input, thus allowing any posterior sampling method to be accommodated. (#5)
  + Added print, plot, as.data.frame methods for "mcmcRocPrc" objects created with mcmcRocPrc(). (#32)
* Fixed several errors in documentation code examples. (#64)
* Added README.rmd to render images.
  + Updated plots to a minimalist theme in documentation
* Updated master branch name to main
* Moved from Travis to GH-Actions

# BayesPostEst 0.3.0

* Planned: Created website through pkgdown
* Added: `plot` method for `mcmcFD` objects and warn that `mcmcFDplot` is deprecated

# BayesPostEst 0.2.1

* Added 'stringsAsFactors = TRUE' for new data.frame() default with R 4.0.0

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
