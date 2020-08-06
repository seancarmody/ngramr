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
  np.rows <- dim(df)[1] - rows

  if (all(c("Phrase", "Corpus", "Year") %in% names(x))) {
    cli::cat_line("# Ngram data table", col = "green")
    cli::cat_line("# Phrases:\t\t", paste(levels(x$Phrase), collapse = ", "))
    cli::cat_line("# Case-sensitive:\t", attributes(x)$case_sensitive)
    cli::cat_line("# Corpuses:\t\t", paste(levels(x$Corpus), collapse = ", "))
    cli::cat_line("# Smoothing:\t\t", attributes(x)$smoothing)
    cli::cat_line("# Years:\t\t", min(x$Year), "-", max(x$Year))
    cat("\n")
  }

  print(utils::head(as.data.frame(df), rows))
  if (np.rows > 0) {
    cli::cat_line(cli::cli_text(cli::col_grey("# ... with {np.rows} more row{?s}")))
  }
  invisible(x)
}

#' @export
`[.ngram` <- function(x, ...) {
  class(x) <- class(x)[-1]
  x <- x[...]
  if (all(c("Phrase", "Corpus", "Year") %in% names(x))) class(x) <- c("ngram", class(x))
  return(x)
}
