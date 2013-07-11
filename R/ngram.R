#' Get n-gram frequencies
#'
#' \code{ngram} downloads data from the Google Ngram Viewer website and
#' returns it in a dataframe.
#'
#' @param phrases vector of phrases
#' @param corpus Google corpus to search (see Details for possible values)
#' @param year_start start year, default is 1500
#' @param year_end end year, default is 2008
#' @param smoothing smoothing paramater, default is 3
#' @param wide a logical value indicating whether results should be returned 
#'   in a "wide" format (phrases are column names) or not. The default is \code{FALSE}.
#' @details 
#'  Google generated two datasets drawn from digitised books in the Google
#'  books collection. One was generated in July 2009, the second in July 2012.
#'  Google will update these datasets as book scanning continues.
#'  
#' Possible corpora: 
#'   \code{eng_2012}, \code{eng_2009}, \code{eng_us_2012}, \code{eng_us_2009}, 
#'   \code{eng_gb_2012}, \code{eng_gb_2009}, \code{chi_sim_2012},
#'   \code{chi_sim_2009}, \code{fre_2012}, \code{fre_2009}, \code{ger_2012},
#'   \code{ger_2009}, \code{spa_2012}, \code{spa_2009}, \code{rus_2012},
#'   \code{rus_2009}, \code{heb_2012}, \code{heb_2009}, \code{ita_2012},  
#'   \code{eng_fiction_2012}, \code{eng_fiction_2009}, \code{eng_1m_2009} 
#' @examples 
#' freq <- ngram(c("mouse", "rat"), year_start=1950)
#' head(freq)
#' @export

ngram <- function(phrases, corpus='eng_2012', year_start=1500,
                  year_end=2008, smoothing=3, wide=FALSE) {
  dfs <- lapply(corpus, function(corp) ngram_single(phrases, corpus=corp,
                                                    year_start=year_start,
                                                    year_end=year_end,
                                                    smoothing=smoothing))
  result <- do.call("rbind", dfs)
  result$Corpus <- as.factor(result$Corpus)
  if (wide) result <- dcast(result, Year + Corpus ~ Phrase, value.var="Frequency")
  return(result)
}

ngram_single <- function(phrases, corpus,...){
  corpus_n <- get_corpus(corpus)
  if (is.na(corpus_n)) {
    warning("Invalid corpus name. Defaulting to 'eng_2012'", call.=FALSE)
    corpus <- "eng_2012"
  }
  df <- ngram_fetch(phrases, corpus_n, ...)
  df$Corpus <- corpus
  return(df)
}

ngram_fetch <- function(phrases, corpus, year_start,  year_end, smoothing) {
  query <- as.list(environment())
  query$phrases <- NULL
  phrases <- phrases[phrases != ""]
  if (length(phrases)==0) stop("No valid phrases provided.")
  ng_url <- ngram_url(phrases, query)
  conn <- url(ng_url)
  html <- readLines(conn)
  close(conn)
  result <- ngram_parse(html)
  result <- melt(result, id.vars="Year", variable.name="Phrase", value.name="Frequency")
  return(result)
}

ngram_url <- function(phrases, query=character()){
  url <- 'http://books.google.com/ngrams/graph'
  phrases <- paste(curlEscape(str_trim(phrases)), collapse='%2C')
  if (phrases=="") stop("No valid phrases provided.")
  url <- paste0(url, "?content=", phrases) 
  if (length(query) > 0) url <- modify_url(url, query=query)
  return(url)
}

ngram_parse <- function(html){
  if (any(grepl("No valid ngrams to plot!<br>", html))) stop("No valid ngrams.") 
    
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
  return(unname(corpora[corpus]))
}