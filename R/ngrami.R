#' Get n-gram frequencies (case insensitive version)
#'
#' @param phrases vector of phrases
#' @param aggregate sum up each of the terms
#' @param ... remaining parameters passed to ngram
#' @export
     
ngrami <- function(phrases, aggregate = TRUE, ...) {
  if (any(grepl("/|\\+| - |_", phrases))) {
    stop("Complex operators not supported for case insensitive search.")
  }
  result <- ngram(phrases, ..., case_ins = TRUE)
  phrases <- tolower(phrases)
  smoothing <- attr(result, "smoothing")
  if (aggregate) {
    phrases <- sort(phrases)
    result$Phrase <- factor(tolower(result$Phrase))
    if (identical(levels(result$Phrase), tolower(phrases))) {
      levels(result$Phrase) <- phrases
    }
    result <- summarise(group_by(result, .data$Year,
                                 .data$Corpus, .data$Phrase),
                        Frequency = sum(.data$Frequency))
  }
  class(result) <- c("ngram", class(result))
  attr(result, "smoothing") <- smoothing
  attr(result, "case_sensitive") <- FALSE
  return(result)
}

ngrami_new <- function(phrases, aggregate = TRUE, ...){
  ngram_new(phrases, aggregate = aggregate, case_ins = TRUE, drop_all = TRUE, ...)  
}
