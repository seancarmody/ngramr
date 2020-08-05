#' Get n-gram frequencies (case insensitive version)
#'
#' @param phrases vector of phrases
#' @param aggregate sum up each of the terms
#' @param ... remaining parameters passed to ngram
#' @export
     
ngrami <- function(phrases, aggregate = TRUE, ...){
  ngram(phrases, aggregate = aggregate, case_ins = TRUE, drop_all = TRUE, ...)  
}
