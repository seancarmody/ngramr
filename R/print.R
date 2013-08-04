#' Print n-gram contents
#' 
#' @param x ngram object as returned by \code{link{ngram}}
#' @param rows number of rows to print from top and bottom of ngram data set. Default is 6.
#' @param ... additional parameters passed to default print method.
#' @export
#' @method print ngram
#' @examples
#' x <- ngram(c("hacker", "programmer"), year_start = 1950)
#' x
#' print(x, rows = 3)

print.ngram <- function(x, rows = 6, ...) {
  cat(paste("Phrases:", paste(levels(x$Phrase), collapse=", "), "\n"))
  cat(paste("Case-sentitive:", attributes(x)$case_sensitive), "\n")
  cat(paste("Corpuses:", paste(levels(x$Corpus), collapse=", "), "\n"))
  cat(paste("Smoothing:", attributes(x)$smoothing), "\n")
  cat("\n")
  
  df <- as.data.frame(x)
  ng.cat <- capture.output(print(df, right=FALSE))
  ng.len <- length(ng.cat)
  if (ng.len > 2 * rows) ng.cat <- c(ng.cat[1:(rows + 1)], 
                                      c("---"), 
                                      ng.cat[(ng.len - rows + 1):ng.len])
  cat(paste(ng.cat, collapse="\n"))
  invisible(x)
}