#' Get n-gram frequencies ("wide" format)
#'
#' @param phrases vector of phrases
#' @param ignore_case ignore case of phrases (i.e. call \code{ngrami}
#'   rather than \code{ngram}). Default value is \code{FALSE}.
#' @param ... remaining parameters passed to \code{ngram}
#' @export

ngramw <- function(phrases, ignore_case=FALSE, ...) {
  if ("ngram" %in% class(phrases)) {
    ng <- phrases
  } else {
    ng <- if (ignore_case) ngrami(phrases, ...) else ngram(phrases, ...)
  }
  ng <- pivot_wider(ng, names_from = "Phrase", values_from = "Frequency")
  return(ng)
}
