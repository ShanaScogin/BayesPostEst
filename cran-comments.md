## Test environments
* major OS tested (gh-actions)
* win-builder (R-release, R-devel, R-oldrelease)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Winbuilder check results
Notes exist in winbuilder develop and checkrhub about the ICPSR link and some doi links including: 10.2307/2669316 and 10.1037/a0029146. All links have been checked and are correct.

## Resubmission note
Fourth attempt at resubmitting. All tests that are used in testing (but not for users) added to depends to avoid all issues. This includes: R2jags, runjags, rstanarm, rjags, MCMCpack, R2WinBUGS, brms. devtools::check(pkg = ".", env_vars = c(R_CHECK_DEPENDS_ONLY_="true")) on my local is passing with 0 ERRORs, WARNINGs, or NOTEs. Winbuilder and other tests still passing all the same.