#' Print n-gram contents
#' 
#' @param ngram ngram object as returned by \code{link{ngram}}
#' @param rows number of rows to print from top and bottom of ngram data set. Default is 6.
#' @export
#' @examples
#' x <- ngram(c("hacker", "programmer"), year_start = 1950)
#' x
#' print(x, rows = 3)

print.ngram <- function(ngram, rows = 6) {
  cat(paste("Phrases:", paste(levels(ngram$Phrase), collapse=", "), "\n"))
  cat(paste("Corpuses:", paste(levels(ngram$Corpus), collapse=", "), "\n"))
  cat(paste("Smoothing:", attributes(ngram)$smoothing), "\n")
  cat("\n")
  
  df <- as.data.frame(ngram)
  ng.cat <- capture.output(print(df, right=FALSE))
  ng.len <- length(ng.cat)
  if (ng.len > 2 * rows) ng.cat <- c(ng.cat[1:(rows + 1)], 
                                      c("---"), 
                                      ng.cat[(ng.len - rows + 1):ng.len])
  cat(paste(ng.cat, collapse="\n"))
  invisible(ngram)
}