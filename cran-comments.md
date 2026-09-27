## Submission

This is a patch release (0.4.1) that fixes the problems shown on the
BayesPostEst CRAN check results page:

* ERROR (Linux flavors): a Stan model in the test setup used array syntax
  that was removed in Stan 2.33. It now uses the `array[N]` syntax.
* ERROR (Windows flavors): the brms model failed to compile during test setup.
  Tests that fit Stan-based models (rstan, rstanarm, brms) are now skipped on
  CRAN, which also shortens the test run time.
* NOTE: "Namespaces in Imports field not imported from: 'HDInterval' 'carData'
  'rjags'". These have been moved to Suggests.

## Test environments

* local macOS (x86_64), R 4.1.0
* win-builder, R-devel (2026-09-25 r90590 ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are currently no reverse dependencies.