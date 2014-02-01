#' Get n-gram frequencies (case insensitive version)
#'
#' @param phrases vector of phrases
#' @param aggregate sum up each of the terms
#' @param ... remaining parameters passed to ngram
#' @export
     
ngrami <- function(phrases, aggregate=TRUE, ...){
  if (any(grepl("/|\\+| - ", phrases))) stop("Complex operators not supported for case insensitive search.")
  phrases_all <- sapply(phrases, function(x) paste0(toupper(substr(x, 1, 1)),
                                                tolower(substring(x, 2)))) 
  phrases_all <- c(phrases, phrases_all, tolower(phrases), toupper(phrases))
  phrases_all <- unique(phrases_all)
  result <- ngram(phrases_all, ...)
  browser()
  smoothing <- attr(result, "smoothing")
  if (aggregate){
    phrases <- sort(phrases)
    result$Phrase <- factor(tolower(result$Phrase))
    if (identical(levels(result$Phrase), tolower(phrases))) levels(result$Phrase) <- phrases 
    result <- ddply(result, c("Year", "Corpus", "Phrase"), summarise, Frequency = sum(Frequency))
  }
  class(result) <- c("ngram", class(result))
  attr(result, "smoothing") <- smoothing
  attr(result, "case_sensitive") <- FALSE
  return(result)
}

if(getRversion() >= "2.15.1") utils::globalVariables(c("Frequency"))
