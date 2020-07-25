#' Print n-gram contents
#' 
#' @param x ngram object as returned by \code{link{ngram}}
#' @param rows number of rows to print. Default is 6.
#' @param ... additional parameters passed to default print method.
#' @export
#' @method print ngram
#' @examples
#' x <- ngram(c("hacker", "programmer"), year_start = 1950)
#' print(x)

print.ngram <- function(x, rows=6, ...) {
  df <- x
  class(df) <- class(df)[-1]
  
  if (all(c("Phrase", "Corpus", "Year") %in% names(x))) {
    cat(paste("Phrases:\t", paste(levels(x$Phrase), collapse=", "), "\n"))
    cat(paste("Case-sentitive:\t", attributes(x)$case_sensitive), "\n")
    cat(paste("Corpuses:\t", paste(levels(x$Corpus), collapse=", "), "\n"))
    cat(paste("Smoothing:\t", attributes(x)$smoothing), "\n")
    cat(paste("From:\t\t", min(x$Year)), "\n")
    cat(paste("To:\t\t", max(x$Year)), "\n")
    
    cat("\n")
  }
  if (class(df)[1] == "tbl_df"){
    print(df, n = rows, ...)
  } else {
    print(head(df, rows))
  }
  invisible(x)
}

#' @export
`[.ngram` <- function(x, ...){
  class(x) <- class(x)[-1]
  x[...]
}
