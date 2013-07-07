#' Get n-gram frequencies (wide format)
#'
#' @param phrases vector of phrases
#' @param case_sensitive Boolean - combine results for upper and lower case?
#' @param ... remaining parameters passed to ngram
#' @details Possible corpora: 
#'   eng_2012, eng_2009, eng_us_2012, eng_us_2009, eng_gb_2012, eng_gb_2009,  
#'   chi_sim_2012, chi_sim_2009, fre_2012, fre_2009, ger_2012, ger_2009, 
#'   spa_2012, spa_2009, rus_2012, rus_2009, heb_2012, heb_2009, ita_2012,  
#'   eng_fiction_2012, eng_fiction_2009, eng_1m_2009 
#' @import reshape2
#' @export

ngramw <- function(phrases, case_sensitive=TRUE, ...){
  results <- if (case_sensitive) ngram(phrases, ...) else ngrami(phrases, ...)
  return(dcast(results, Year + Corpus ~ Term, value.var="Frequency"))
}