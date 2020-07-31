#' Get n-gram frequencies
#'
#' \code{ngram} downloads data from the Google Ngram Viewer website and
#' returns it in a dataframe.
#'
#' @param phrases vector of phrases, with a maximum of 12 items
#' @param corpus Google corpus to search (see Details for possible values)
#' @param year_start start year, default is 1500
#' @param year_end end year, default is 2008
#' @param smoothing smoothing paramater, default is 3
#' @param count logical, denoting whether phrase counts should be returned as
#'   well as frequencies. Default is \code{FALSE}.
#' @param tag apply a part-of-speech tag to the whole vector of phrases
#' @param case_ins Logical indicating whether to force a case insenstive search. 
#'   Default is \code{FALSE}.
#' @details 
#'  Google generated two datasets drawn from digitised books in the Google
#'  Books collection. One was generated in July 2009, the second in July 2012.
#'  Google will update these datasets as book scanning continues.
#'  
#'  This function provides the annual frequency of words or phrases, known as
#'  n-grams, in a sub-collection or "corpus" taken from the Google Books collection.
#'  The search across the corpus is case-sensitive. For a case-insensitive search
#'  use \code{\link{ngrami}}. 
#'  
#' Below is a list of available corpora.
#' \tabular{ll}{
#' \bold{Corpus} \tab \bold{Corpus Name}\cr
#' eng_us_2019\tab American English 2019\cr
#' eng_us_2012\tab American English 2012\cr
#' eng_us_2009\tab American English 2009\cr
#' eng_gb_2019\tab British English 2019\cr
#' eng_gb_2012\tab British English 2012\cr
#' eng_gb_2009\tab British English 2009\cr
#' chi_sim_2019\tab Chinese 2019\cr
#' chi_sim_2012\tab Chinese 2012\cr
#' chi_sim_2009\tab Chinese 2009\cr
#' eng_2019\tab English 2019\cr
#' eng_2012\tab English 2012\cr
#' eng_2009\tab English 2009\cr
#' eng_fiction_2019\tab English Fiction 2019\cr
#' eng_fiction_2012\tab English Fiction 2012\cr
#' eng_fiction_2009\tab English Fiction 2009\cr
#' eng_1m_2009\tab Google One Million\cr
#' fre_2019\tab French 2019\cr
#' fre_2012\tab French 2012\cr
#' fre_2009\tab French 2009\cr
#' ger_2019\tab German 2019\cr
#' ger_2012\tab German 2012\cr
#' ger_2009\tab German 2009\cr
#' heb_2019\tab Hebrew 2019\cr
#' heb_2012\tab Hebrew 2012\cr
#' heb_2009\tab Hebrew 2009\cr
#' spa_2019\tab Spanish 2019\cr
#' spa_2012\tab Spanish 2012\cr
#' spa_2009\tab Spanish 2009\cr
#' rus_2019\tab Russian 2019\cr
#' rus_2012\tab Russian 2012\cr
#' rus_2009\tab Russian 2009\cr
#' ita_2019\tab Italian 2019\cr
#' ita_2012\tab Italian 2012\cr
#' }
#' 
#' The Google Million is a sub-collection of Google Books. All are in
#' English with dates ranging from 1500 to 2008.
#' No more than about 6,000 books were chosen from any one year, which means that
#' all of the scanned books from early years are present, and books from later
#' years are randomly sampled. The random samplings reflect the subject distributions
#' for the year (so there are more computer books in 2000 than 1980).
#' 
#' See \url{http://books.google.com/ngrams/info} for the full Ngram syntax.
#' @examples 
#' freq <- ngram(c("mouse", "rat"), year_start = 1950)
#' head(freq)
#' freq <- ngram(c("blue", "red"), tag = "ADJ")
#' head(freq)
#' freq <- ngram(c("President Roosevelt", "President Truman"), tag = "START", year_start = 1920)
#' head(freq)
#' @export

ngram <- function(phrases, corpus='eng_2019', year_start = 1500,
                  year_end = 2020, smoothing = 3, count=FALSE,
                  tag = NULL, case_ins=FALSE) {
  stopifnot(is.character(phrases))
  if (!all(check_balanced(phrases))) stop("Mis-matched parentheses")
  if (length(phrases) > 12){
    phrases <- phrases[1:12]
    warning("Maximum number of phrases exceeded: only using first 12.")
  }
  dfs <- lapply(corpus, function(corp) ngram_single(phrases, corpus=corp,
                                                    year_start=year_start,
                                                    year_end=year_end,
                                                    smoothing=smoothing,
                                                    tag=tag, case_ins))
  result <- do.call("rbind", dfs)
  result$Corpus <- as.factor(result$Corpus)
  class(result) <- c("ngram", class(result))
  attr(result, "smoothing") <- smoothing
  attr(result, "case_sensitive") <- TRUE
  result$Phrase <- factor(result$Phrase)
  if (count) result <- add_count(result)
  return(result)
}

ngram_new <- function(phrases, corpus=26, year_start = 1800,
                  year_end = 2020, smoothing = 3, case_ins=FALSE,
                  count = FALSE,
                  aggregate = FALSE, clean = TRUE) {
  query <- as.list(environment())
  if (case_ins) query["case_insensitive"] <- "on"
  query$phrases <- NULL
  query$case_ins <- NULL
  phrases <- phrases[phrases != ""]
  if (length(phrases)==0) stop("No valid phrases provided.")
  ng_url <- ngram_url(phrases, query)
  html <- ngram_fetch_xml(ng_url)
  ng <- ngram_fetch_data(html)
  warnings <- ngram_check_warnings(html)
  if (length(warnings) > 0) {
    for (w in warnings){
      warning(w$message, call. = FALSE)
    }
    attr(ng, "warnings") <- warnings
  }
  if (aggregate) ng <- filter(ng, .data$type != 'EXPANSION')
  if (clean) ng <- mutate(ng, Phrase = .data$clean)
  ng <- select(ng, -.data$clean)
  class(ng) <- c("ngram", class(ng))
  attr(ng, "smoothing") <- smoothing
  attr(ng, "case_sensitive") <- TRUE
  ng$Phrase <- factor(ng$Phrase)
  ng$Corpus <- as.factor(ng$Corpus)
  if (count) ng <- add_count(ng)
  return(ng)
}

ngram_single <- function(phrases, corpus, tag, case_ins, ...){
  phrases <- phrases[1:ifelse(length(phrases) < 13, length(phrases), 12)]
  if (!is.null(tag)) {
    if (grepl("NOUN|VERB|ADJ|ADV|PRON|DET|ADP|NUM|CONJ|PRT", tag))
      phrases = paste0(phrases, "_", gsub("_", "", tag))      
    else if (grepl("ROOT|START|END", tag))
      phrases = paste(paste0("_", tag, "_"), phrases)      
  }
  corpus_n <- get_corpus(corpus)
  if (is.na(corpus_n)) {
    warning("Invalid corpus name. Defaulting to 'eng_2019'", call.=FALSE)
    corpus_n <- get_corpus("eng_2019")
  }
  df <- ngram_fetch(phrases, corpus_n, case_ins,...)
  if (NROW(df) > 0){
    df <- tidyr::pivot_longer(df, -.data$Year, names_to="Phrase", values_to="Frequency")
    df$Phrase <- textutils::HTMLdecode(df$Phrase)
    df$Corpus <- corpus
  }
  return(df)
}

ngram_fetch <- function(phrases, corpus, year_start,  year_end, smoothing, case_ins=FALSE) {
  # Retrieve HTML data
  query <- as.list(environment())
  if (case_ins) query["case_insensitive"] <- "on"
  query$phrases <- NULL
  query$case_ins <- NULL
  phrases <- phrases[phrases != ""]
  if (length(phrases)==0) stop("No valid phrases provided.")
  ng_url <- ngram_url(phrases, query)
  html <- ngram_fetch_html(ng_url)
  result <- ngram_parse(html)
  return(result)
}

ngram_fetch_html <- function(url){
  html <- strsplit(httr::content(httr::GET(url), "text"), "\n", perl=TRUE)[[1]]
  if (html[1] == "Please try again later.") stop('Server busy, answered "Please try again later."')
  return(html)  
}

ngram_fetch_xml <- function(url, text = FALSE){
  if (text) {
    html <- httr::content(httr::GET(url), "text")
    } else html <- xml2::read_html(url)
  return(html)
}

ngram_check_warnings <- function(html){
  node <- xml2::xml_find_first(html, "//div[@id='warning-area']")
  warnings <- list()
  if (length(node) > 0) {
    for (n in xml2::xml_find_all(node, "div")){
      type <- xml2::xml_text(xml2::xml_find_first(n, "mwc-icon"))
      msg <- stringr::str_trim(xml2::xml_text(xml2::xml_find_first(n, "span")))
      warnings <- c(warnings, list(list(type = type, message = msg)))
    }
  }
  return(warnings)
}

ngram_fetch_data <- function(html, debug = FALSE){
  corpus <- xml2::xml_find_first(html, "//select[@id='form-corpus']/option")
  corpus <- as.integer(xml2::xml_attr(corpus, "value"))
  json <- xml2::xml_find_first(html, "//div[@id='chart']/following::script")
  json <- xml2::xml_text(json)
  json <- stringr::str_split(json, "\n")[[1]]
  json <- stringr::str_trim(json)
  years <- as.integer(stringr::str_split(grep("drawD3Chart", json, value=TRUE), ",")[[1]][2:3])
  if (debug) return(list(json=json, years=years, corpus=corpus))
  json <- grep("ngrams.data", json, value = TRUE)
  data <- rjson::fromJSON(sub(".*?=", "", json))
  if (length(data) == 0) return(NULL)
  data <- lapply(data,
                 function(x) tibble::add_column(tibble::as_tibble(x), Year = seq.int(years[1], years[2])))
  data <- bind_rows(data)
  data <- mutate(data, Corpus = get_corpus(.data$corpus, text=FALSE))
  data <- separate(data, ngram, c("clean", "C"), ":", remove=FALSE, extra = "drop", fill="right")
  data <- mutate(data, n = get_corpus(.data$C), Corpus = if_else(is.na(n), .data$Corpus, .data$C), C = NULL, n = NULL)
  data <- dplyr::relocate(data, .data$Year, .data$ngram, .data$timeseries, .data$Corpus)
  data <- dplyr::rename(data, Phrase = .data$ngram, Frequency = .data$timeseries)
  return(data)
}

ngram_url <- function(phrases, query=character()){
  url <- 'https://books.google.com/ngrams/graph'
  n <- length(phrases)
  for (i in 1:n){
    if (grepl("\\+|/", phrases[i])) phrases[i] <- paste0("(", phrases[i], ")")
    p <- phrases[i]
    if (!(Encoding(p) %in% c("unknown", "UTF-8"))){
      phrases[i] <- iconv(p, Encoding(p), "UTF-8")
    }   
  }
  phrases <- paste(RCurl::curlEscape(stringr::str_trim(phrases)), collapse='%2c')
  if (phrases=="") stop("No valid phrases provided.")
  url <- paste0(url, "?content=", phrases) 
  if (length(query) > 0) url <- httr::modify_url(url, query=query)
  url <- gsub("%28", "(", url)
  url <- gsub("%29", ")", url)
  url <- gsub("%20", "+", url)
  return(url)
}

ngram_parse <- function(html){
   if (any(grepl("No valid ngrams to plot!<br>", html))) return(data.frame())
  # Warn about character substitution
  lapply(grep("^Google has substituted ",
              gsub("<.?b.?>","", sub("Replaced (.*) to match how we processed the books",
                                              "Google has substituted \\1", html)),
              value=TRUE), warning, call. = FALSE)  
  data_line <- grep("ngrams.data", html)
  year_line <- grep("drawD3Chart", html)
  warn_line <- grep("class=\"warning-text\"", html)
  if (length(warn_line) > 0){
    warning_message = textutils::HTMLdecode(html[warn_line + 1])
    warning_message = stringr::str_trim(warning_message)
    cli::cat_line("Warning: ", warning_message, col="red")
  }
  data = html[data_line]
  if (gsub(" ", "", data[1]) == "ngrams.data=[];") stop("no data returned")
  ngram_data <- rjson::fromJSON(sub(".*?=", "", data))
  ngram_data <- ngram_data[unlist(lapply(ngram_data, function(x) !("type" %in% names(x)) || x$type != 'ALTERNATE_FORM'))]
  years <- as.integer(strsplit(html[year_line], ",")[[1]][2:3])
  cols <- unlist(lapply(ngram_data, function(x) x$ngram))
  data <- as.data.frame(lapply(ngram_data[lapply(ngram_data, length) > 0],
                               function(x) x$timeseries))
  years <- seq.int(years[1], years[2])
  if (NROW(data)==0) return(data.frame())
  data <- cbind(years, data)
  colnames(data) <- c("Year", cols)
  data <- data[!grepl("\\*|\\(All\\)", names(data))]
  return(data)
}

get_corpus <- function(corpus, text = TRUE){
  corpora <- c('eng_us_2012'=17, 'eng_us_2009'=5, 'eng_gb_2012'=18, 'eng_gb_2009'=6, 
           'chi_sim_2012'=23, 'chi_sim_2009'=11,'eng_2012'=15, 'eng_2009'=0,
           'eng_fiction_2012'=16, 'eng_fiction_2009'=4, 'eng_1m_2009'=1, 'fre_2012'=19,
           'fre_2009'=7, 'ger_2012'=20, 'ger_2009'=8, 'heb_2012'=24, 'heb_2009'=9, 
           'spa_2012'=21, 'spa_2009'=10, 'rus_2012'=25, 'rus_2009'=12, 'ita_2012'=22,
           'eng_2019'=26, 'eng_us_2019'=28, 'eng_gb_2019'=29, 'eng_fiction_2019'=27,
           'chi_sim_2019'=34, 'fre_2019'=30, 'ger_2019'=31, 'heb_2019'=35, 'ita_2019'=33,
           'rus_2019'=36, 'spa_2019'=32)
  if (text) return(unname(corpora[corpus])) else return (names(which(corpora == corpus)))
}

check_balanced <- function(x){
  # Check parenthesis are appropriately balanced (i.e. every open is closed)
  sapply(x, function(str) {
    str <- gsub("[^\\(\\)]", "", str)
    str <- strsplit(str, "")[[1]]
    str <- ifelse(str=='(', 1, -1)
    all(cumsum(str) >= 0) && sum(str) == 0
  })
}
