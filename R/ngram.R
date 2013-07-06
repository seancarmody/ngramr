
#' Get n-gram frequencies
#'
#' @param phrases vector of phrases
#' @param corpus Google corpus to search
#' @param start_year start year
#' @import RCurl httr rjson
#' @export

ngram <- function(phrases, corpus=NULL, start_year=1900) {
  query <- list(start_year=start_year)
  ng_url <- ngram_url(phrases, query)
  conn <- url(ng_url)
  html <- readLines(conn)
  close(conn)
  ngram_parse(html)
}

ngram_url <- function(phrases, query=character()){
  url <- 'http://books.google.com/ngrams/graph'
  phrases <- paste(curlEscape(phrases), collapse=',')
  url <- modify_url(url, query=list(content=phrases)) 
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