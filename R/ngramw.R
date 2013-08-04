#' Get n-gram frequencies ("wide" format)
#'
#' @param phrases vector of phrases
#' @param ignore_case ignore case of phrases (i.e. call \code{ngrami} rather than
#'   \code{ngram}). Default value is \code{FALSE}.
#' @param ... remaining parameters passed to \code{ngram}
#' @export

ngramw <- function(phrases, ignore_case=FALSE, ...) {
  ng <- if (ignore_case) ngrami(phrases, ...) else ngram(phrases, ...) 
  class(ng) <- "data.frame"
  ng <- dcast(ng, Year + Corpus ~ Phrase, value.var="Frequency")
  return(ng)
}