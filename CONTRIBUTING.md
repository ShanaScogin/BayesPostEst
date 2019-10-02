# Contributing to `BayesPostEst`

We appreciate your interest in contributing to our project. This document contains general guidelines for this process. We welcome contributions from all, regardless of your level of experience. To see the enhancements we have planned or are working on, check out our current [ issues](https://github.com/ShanaScogin/BayesPostEst/issues). [Johannes Karreth's website](http://www.jkarreth.net) has more resources on Bayesian estimation that might help you gain an idea of what this package offers and where it might go in the future.

By participating in this project, you agree to abide by the [code of conduct](CODE_OF_CONDUCT.md).

# Types of contributions 

Contributions can come in many forms. This includes (but is not limited to!):

- Identifying areas for future development ([open an Issue](https://github.com/ShanaScogin/BayesPostEst/issues))
- Identifying issues/bugs in the code ([open an Issue](https://github.com/ShanaScogin/BayesPostEst/issues))
- Adding functionality ([open a Pull Request](https://github.com/ShanaScogin/BayesPostEst/pulls))
- Fixing bugs ([open a Pull Request](https://github.com/ShanaScogin/BayesPostEst/pulls))

# Code formatting
- In general, `BayesPostEst` uses camelCase for function names.
- Functions are generally named with lowercase `mcmc` followed by a description of its functionality, such as `mcmcFD` for the first differences of a Bayesian logit or probit model estimation or `mcmcRocPrc` for the ROC and precision-recall curves of a Bayesian logit or probit model estimation.
- For coding style within functions other than naming, BayesPostEst currently defaults to the [tidyverse style](https://style.tidyverse.org/). A more detailed design philosophy will be available in the near future.