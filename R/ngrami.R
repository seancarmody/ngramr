#' Get n-gram frequencies (case insensitive version)
#'
#' @param phrases vector of phrases
#' @param aggregate sum up each of the terms
#' @param ... remaining parameters passed to ngram
#' @details Possible corpora: 
#'   eng_2012, eng_2009, eng_us_2012, eng_us_2009, eng_gb_2012, eng_gb_2009,  
#'   chi_sim_2012, chi_sim_2009, fre_2012, fre_2009, ger_2012, ger_2009, 
#'   spa_2012, spa_2009, rus_2012, rus_2009, heb_2012, heb_2009, ita_2012,  
#'   eng_fiction_2012, eng_fiction_2009, eng_1m_2009 
#' @export
     
ngrami <- function(phrases, aggregate=TRUE, wide=FALSE, ...){
  phrases <- sapply(phrases, function(x) paste0(toupper(substr(x, 1, 1)),
                                                tolower(substring(x, 2)))) 
  phrases <- c(phrases, tolower(phrases), toupper(phrases))
  result <- ngram(phrases, wide=FALSE, ...)
  if (aggregate){
    result$Phrase <- tolower(result$Phrase)
    result <- ddply(result, .(Year, Corpus, Phrase), summarise, Frequency = sum(Frequency))
    result$Phrase <- factor(result$Phrase)
  }
  if (wide) result <- dcast(result, Year + Corpus ~ Phrase, value.var="Frequency")
  return(result)
}