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
Third attempt at resubmitting. R2jags added to depends to avoid all issues. Checks with R_CHECK_DEPENDS_ONLY=true pass now with and without R2jags on local.