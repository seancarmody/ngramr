## Test environments
* local MacOS install, R 4.0.2
* win-builder (devel and release)
* [Travis CI](http://travis.ci/)

## R CMD check results
* There were no ERRORs or WARNINGs
* Local and Travis CI builds generatoed no NOTES. 
* Win-builder generated 1 NOTE
  * Possibly mis-spelled words in DESCRIPTION. The spelling ("Ngram") is correct

## Downstream dependencies
Reverse dependencies checked with devtools::revdep(). No dependencies or issues found.
