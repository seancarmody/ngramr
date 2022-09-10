## Test environments
* local MacOS install, R 4.2.1
* rhub
* win-builder (devel and release)

## R CMD check results
* There were no ERRORs or WARNINGs
* Local build generated no NOTES. 
* Local and Travis CI builds generated no NOTES. 
* Addressed CRAN submission requirements as follows:
  * Fixed multiple errors that arose after changes to the Google ngram viewer website
  * Note that I have not included Jean-Baptiste Michel in the Authors@R list. Jean-Baptiste wrote some python code which provided the original inspiration for this package. Since then the code has been significantly re-written. I don't see an appropriate role type to include a reference to Jean-Baptiste within the Authors@R list.

## Downstream dependencies
Reverse dependencies checked with devtools::revdep(). No dependencies or issues found.
