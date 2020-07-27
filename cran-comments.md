## Test environments
* local MacOS install, R 4.0.2
* win-builder (devel and release)
* (travis)[http://travis.ci/]

## R CMD check results
* There were no ERRORs or WARNINGs
* Local and travis generatoed no NOTES. 
* Win-builder generated 1 NOTE
  * Possibly mis-spelled words in DESCRIPTION. The spelling ("Ngram") is correct

## Downstream dependencies
Reverse dependencies checked with devtools::revdep_check(). No dependencies or issues found.
