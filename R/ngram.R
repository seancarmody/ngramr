#' Get n-gram url
#' #'
#' @param phrases vector of phrases
#' @param query list of additional query parameters
#' @import RCurl httr

ngram_url <- function(phrases, query=character()){
  url <- 'http://books.google.com/ngrams/graph'
  phrases <- paste(curlEscape(phrases), collapse=',')
  url <- modify_url(url, query=list(content=phrases)) 
  if (length(query) > 0) url <- modify_url(url, query=query)
  return(url)
}

#' Get n-gram frequencies
#'
#' @param phrases vector of phrases
#' @param corpus Google corpus to search
#' @param start_year start year
#' @export

ngram <- function(phrases, corpus=NULL, start_year=1900) {
  query <- list(start_year=start_year)
  ng_url <- ngram_url(phrases, query)
  conn <- url(ng_url)
  ng_page <- readLines(conn)
  close(conn)
  return(ng_page)
}