#' Get n-gram frequencies
#'
#' @param phrases vector of phrases
#' @param corpus Google corpus to search
#' @param start_year start year, default is 1500
#' @param end_year end year, default is 2008
#' @param smoothing smoothing paramater, default is 3
#' @details Possible corpora: 
#'   eng_2012, eng_2009, eng_us_2012, eng_us_2009, eng_gb_2012, eng_gb_2009,  
#'   chi_sim_2012, chi_sim_2009, fre_2012, fre_2009, ger_2012, ger_2009, 
#'   spa_2012, spa_2009, rus_2012, rus_2009, heb_2012, heb_2009, ita_2012,  
#'   eng_fiction_2012, eng_fiction_2009, eng_1m_2009 
#' @import RCurl httr rjson stringr
#' @export

ngram <- function(phrases, corpus='eng_2012', start_year=1500,
                  end_year=2008, smoothing=3) {
  query <- as.list(environment())
  query$phrases <- NULL
  ng_url <- ngram_url(phrases, query)
  conn <- url(ng_url)
  html <- readLines(conn)
  close(conn)
  ngram_parse(html)
}

ngram_url <- function(phrases, query=character()){
  url <- 'http://books.google.com/ngrams/graph'
  phrases <- paste(curlEscape(str_trim(phrases)), collapse='%2C')
  url <- paste0(url, "?content=", phrases) 
  if (length(query) > 0) url <- modify_url(url, query=query)
  return(url)
}

ngram_parse <- function(html){
  cols <- lapply(strsplit(grep("addColumn", html, value=TRUE), ","), getElement, 2)
  cols <- gsub(".*'(.*)'.*", "\\1", cols)

  html <- paste(html[-(1:grep("data.addRows\\(", html))], collapse='')
  html <- gsub("\\).*", "", html)
  
  data <- as.data.frame(t(sapply(fromJSON(html), unlist)))
  colnames(data) <- cols
  return(data)
}

get_corpus <- function(corpus){
  corpora <- c('eng_us_2012'=17, 'eng_us_2009'=5, 'eng_gb_2012'=18, 'eng_gb_2009'=6, 
           'chi_sim_2012'=23, 'chi_sim_2009'=11,'eng_2012'=15, 'eng_2009'=0,
           'eng_fiction_2012'=16, 'eng_fiction_2009'=4, 'eng_1m_2009'=1, 'fre_2012'=19, 'fre_2009'=7, 
           'ger_2012'=20, 'ger_2009'=8, 'heb_2012'=24, 'heb_2009'=9, 
           'spa_2012'=21, 'spa_2009'=10, 'rus_2012'=25, 'rus_2009'=12, 'ita_2012'=22)
  return(unlist(corporate[corpus]))
}