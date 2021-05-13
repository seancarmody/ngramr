## Test environments
* local MacOS install, R 4.0.5
* win-builder (devel and release)
* [Travis CI](http://travis.ci/)

## R CMD check results
* There were no ERRORs or WARNINGs
* Local and Travis CI builds generated no NOTES. 
* Addressed CRAN submission requirements as follows:
  * Added link in DESCRIPTION to the Google Ngram Viewer webservice
  * Switched a number of examples from \dontrun to \donttest to manage example run time to 5 sec
  * Added Authors@R field
  * Note that I have not included Jean-Baptiste Michel in the Authors@R list. Jean-Baptiste wrote some python code which provided the original inspiration for this package. Since then the code has been significantly re-written. I don't see an appropriate role type to include a reference to Jean-Baptiste within the Authors@R list.

## Downstream dependencies
Reverse dependencies checked with devtools::revdep(). No dependencies or issues found.
