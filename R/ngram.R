#' Get n-gram frequencies
#'
#' @param phrases vector of phrases
#' @param corpus Google corpus to search
#' @import RCurl httr
#' @export

ngram <- function(phrases){
  url <- 'http://books.google.com/ngrams/graph'
  phrases <- paste(curlEscape(phrases), collapse=',')
  url <- modify_url(url, query=list(content=phrases)) 
  return(url)
}