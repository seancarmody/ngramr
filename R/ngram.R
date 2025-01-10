#' Get n-gram frequencies
#'
#' `ngram` downloads data from the Google Ngram Viewer website and
#' returns it in a tibble.
#'
#' @param phrases vector of phrases, with a maximum of 12 items
#' @param corpus Google corpus to search (see Details for possible values)
#' @param year_start start year, default is 1800. Data available back to 1500.
#' @param year_end end year, default is 2008
#' @param smoothing smoothing parameter, default is 3
#' @param case_ins Logical indicating whether to force a case insensitive search.
#'   Default is `FALSE`.
#' @param aggregate Sum up the frequencies for ngrams associated with wildcard
#'   or case insensitive searches. Default is `FALSE`.
#' @param count Default is `FALSE`.
#' @param drop_parent  Drop the parent phrase associated with a wildcard
#'   or case-insensitive search. Default is `FALSE`.
#' @param drop_all Delete the suffix "(All)" from aggregated case-insensitive
#'   searches. Default is `FALSE`.
#' @param type Include the Google return type (e.g. NGRAM, NGRAM_COLLECTION,
#'   EXPANSION) from result set. Default is `FALSE`.
#' @return `ngram` returns an object of class "`ngram`",
#'   which is a tidyverse `tibble` enriched with attributes reflecting 
#'   some of the parameters used in the Ngram Viewer query.
#' @details
#'  Google generated two datasets drawn from digitised books in the Google
#'  Books collection. One was generated in July 2009, the second in July 2012
#'  and the third in 2019. Google is expected to update these datasets as book
#'  scanning continues.
#'
#'  This function provides the annual frequency of words or phrases, known
#'  as n-grams, in a sub-collection or "corpus" taken from the Google Books
#'  collection.The search across the corpus is case-sensitive.
#'  
#' If the function is unable to retrieve data from the Google Ngram Viewer
#' site (either because of access issues or if the format of Google's site
#' has changed) a NULL result is returned and messages are printed to the 
#' console but no errors or warnings are raised (this is to align with
#' CRAN package policies).
#' 
#' Below is a list of available corpora. Note that the data for the 2012
#' corpuses only extends to 2009.
#' \tabular{ll}{
#' \bold{Corpus} \tab \bold{Corpus Name}\cr
#' en-US-2019\tab American English 2019\cr
#' en-US-2012\tab American English 2012\cr
#' en-US-2009\tab American English 2009\cr
#' en-GB-2019\tab British English 2019\cr
#' en-GB-2012\tab British English 2012\cr
#' en-GB-2009\tab British English 2009\cr
#' zh-Hans-2019\tab Chinese 2019\cr
#' zh-Hans-2012\tab Chinese 2012\cr
#' zh-Hans-2009\tab Chinese 2009\cr
#' en-2019\tab English 2019\cr
#' en-2012\tab English 2012\cr
#' en-2009\tab English 2009\cr
#' en-fiction-2019\tab English Fiction 2019\cr
#' en-fiction-2012\tab English Fiction 2012\cr
#' en-fiction-2009\tab English Fiction 2009\cr
#' en-1M-2009\tab English One Million\cr
#' fr-2019\tab French 2019\cr
#' fr-2012\tab French 2012\cr
#' fr-2009\tab French 2009\cr
#' de-2019\tab German 2019\cr
#' de-2012\tab German 2012\cr
#' de-2009\tab German 2009\cr
#' iw-2019\tab Hebrew 2019\cr
#' iw-2012\tab Hebrew 2012\cr
#' iw-2009\tab Hebrew 2009\cr
#' es-2019\tab Spanish 2019\cr
#' es-2012\tab Spanish 2012\cr
#' es-2009\tab Spanish 2009\cr
#' ru-2019\tab Russian 2019\cr
#' ru-2012\tab Russian 2012\cr
#' ru-2009\tab Russian 2009\cr
#' it-2019\tab Italian 2019\cr
#' it-2012\tab Italian 2012\cr
#' }
#'
#' The Google Million is a sub-collection of Google Books. All are in
#' English with dates ranging from 1500 to 2008.
#' No more than about 6,000 books were chosen from any one year, which
#' means that all of the scanned books from early years are present,
#' and books from later years are randomly sampled. The random samplings
#' reflect the subject distributions for the year (so there are more
#' computer books in 2000 than 1980).
#'
#' See \url{http://books.google.com/ngrams/info} for the full Ngram syntax.
#' @examples
#' \donttest{ngram(c("mouse", "rat"), year_start = 1950)
#' ngram(c("blue_ADJ", "red_ADJ"))
#' ngram(c("_START_ President Roosevelt", "_START_ President Truman"), year_start = 1920)
#' }
#' @export

ngram <- function(phrases, corpus = "en", year_start = 1800, 
                      year_end = 2022, smoothing = 3, case_ins=FALSE,
                      aggregate = FALSE, count = FALSE, 
                      drop_parent = FALSE, drop_all = FALSE, type = FALSE) {
  #if (!curl::has_internet()) {stop("Unable to access internet.")}
  phrases <- ngram_check_phrases(phrases)
  # Loop over corpuses
  dfs <- lapply(corpus, function(corp) ngram_single(phrases, corpus = corp,
                                                    year_start = year_start,
                                                    year_end = year_end,
                                                    smoothing = smoothing,
                                                    case_ins = case_ins))
  ng <- bind_rows(dfs)
  if (length(ng) == 0) return(NULL)
  class(ng) <- c("ngram", class(ng))
  ng <- truncate_years(ng)
  if (aggregate) {
    ng <- filter(ng, .data$type != "EXPANSION")
    } else {
    ng <- filter(ng, .data$type %in% c("NGRAM", "EXPANSION"))
    }
  print(ng)
  if (drop_parent || all(ng$Parent == "")) ng$Parent <- NULL
  if (drop_all) {
    ng <- mutate(ng, 
                 Phrase = if_else(type == "CASE_INSENSITIVE",
                                  stringr::str_replace(.data$Phrase, "\\s*\\(All\\)\\z", ""), 
                                  .data$Phrase))
  }
  #ng <- select(ng, -"clean")
  attr(ng, "smoothing") <- smoothing
  attr(ng, "case_sensitive") <- !case_ins
  ng$Corpus <- as.factor(ng$Corpus)
  ng$Phrase <- as.factor(ng$Phrase)
  if (type) ng$Type <- ng$type
  ng$type <- NULL
  return(ng)
}

ngram_single <- function(phrases, corpus, year_start, year_end,
                             smoothing, case_ins) {
  if (!(corpus %in% corpuses$Shorthand)) {warning(paste(corpus, "not a valid corpus. Defaulting to en-2019."))}
  #corpus <- get_corpus_n(corpus)
  query <- as.list(environment())
  if (case_ins) query["case_insensitive"] <- "true"
  query$phrases <- NULL
  query$case_ins <- NULL
  ng_url <- ngram_url(phrases, query)
  html <- ngram_fetch_xml(ng_url)
  if (is.null(html)){
    ng <- NULL
  } else {
    ng <- ngram_fetch_data(html)
    warnings <- ngram_check_warnings(html)
    show_warnings(warnings)
  }
  return(ng)
}

ngram_check_phrases <- function(phrases){
  stopifnot(is.character(phrases))
  phrases <- phrases[phrases != ""]
  if (length(phrases) == 0) stop("No valid phrases provided.")
  if (!all(check_balanced(phrases))) stop("mis-matched parentheses")
  if (length(phrases) > 12) {
    phrases <- phrases[1:12]
    warning("Maximum number of phrases exceeded: only using first 12.")
  }
  return(phrases)
}

ngram_fetch_xml <- function(url) {
  # retrieve data from Google Ngram Viewer site
  # no errors or warnings generated on fail, only messages
  try_get <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(3), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }
  
  # first check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # then try for timeout problems
  resp <- try_get(url)
  if (!is_response(resp)) {
    message("Please check Google's Ngram Viewer site is up.")
    message(resp)
    return(invisible(NULL))
  }
  # then stop if status > 400
  if (httr::http_error(resp)) { 
    message("Please check Google's Ngram Viewer site is up.")
    httr::message_for_status(resp)
    return(invisible(NULL))
  }
  return(xml2::read_html(resp))
}

ngram_check_warnings <- function(html) {
  node <- xml2::xml_find_first(html, "//div[@id='warning-area']")
  warnings <- list()
  if (length(node) > 0) {
    for (n in xml2::xml_find_all(node, "div")) {
      type <- xml2::xml_text(xml2::xml_find_first(n, "mwc-icon"))
      msg <- stringr::str_trim(xml2::xml_text(xml2::xml_find_first(n, "span")))
      msg <- stringr::str_replace_all(msg, "\\s+", " ")
      msg <- stringr::str_replace(msg, "No valid ngrams to plot!", "No valid ngrams retrieved!")
      warnings <- c(warnings, list(list(type = type, message = msg)))
    }
  }
  return(warnings)
}

ngram_fetch_data <- function(html) {
  data <- tryCatch(
    {
      if (is.null(html)) {
        NULL
      } else {
        corpus <- xml2::xml_find_first(html, "//select[@id='form-corpus']/option")
        corpus <- xml2::xml_attr(corpus, "value")
        if (grepl("^[0-9]+$", corpus, perl = TRUE)) {
          corpus <- get_corpus_text(as.numeric(corpus))
          }
        script <- xml2::xml_find_all(html, "//div[@id='chart']/following::script")[1]
        json <- xml2::xml_text(script)
        json <- stringr::str_split(json, "\n")[[1]]
        json <- json[json != '']
        json <- stringr::str_squish(json)
        years <- xml2::xml_find_all(html, "//div[@id='chart']/following::script")[2]
        years <- xml2::xml_text(years)
        years <-  stringr::str_split(years, "\n")[[1]]
        years <-  grep('drawD3Chart', years, value = TRUE)
        years <- as.integer(stringr::str_split(grep("drawD3Chart", years, value = TRUE), ",")[[1]][2:3])
        data <- rjson::fromJSON(json)
        if (length(data) == 0) return(NULL)
        data <- lapply(data,
                       function(x) tibble::add_column(tibble::as_tibble(x),
                                                      Year = seq.int(years[1], years[2])))
        data <- bind_rows(data)
        data <- mutate(data, ngram = textutils::HTMLdecode(data$ngram), Corpus = corpus)
        data <- relocate(data, "Year", "ngram", "timeseries", "Corpus")
        data <- rename(data, Phrase = "ngram",  Frequency = "timeseries", Parent = "parent")
        data
      }
    },
    error=function(cond) {
      message("Error parsing ngram data, please contact package maintainer.")
      message("Here's the original error message:")
      message(cond)
      message("\nError occurred in the following code:")
      message(conditionCall(cond))
      return(NULL)
    },
    warning=function(cond) {
      message("Warning generated when parsing ngram data.")
      message("Here's the original warning message:")
      message(cond)
      return(NULL)
    },
    finally = {}
  )
  return(data)
}

ngram_url <- function(phrases, query=character()) {
  url <- "https://books.google.com/ngrams/graph"
  n <- length(phrases)
  for (i in 1:n) {
    if (grepl("\\+|/", phrases[i])) phrases[i] <- paste0("(", phrases[i], ")")
    p <- phrases[i]
    if (!(Encoding(p) %in% c("unknown", "UTF-8"))) {
      phrases[i] <- iconv(p, Encoding(p), "UTF-8")
    }
  }
  phrases <- paste(curl::curl_escape(stringr::str_trim(phrases)),
                   collapse = "%2c")
  if (phrases == "") stop("No valid phrases provided.")
  url <- paste0(url, "?content=", phrases)
  if (length(query) > 0) url <- httr::modify_url(url, query = query)
  url <- gsub("%28", "(", url)
  url <- gsub("%29", ")", url)
  url <- gsub("%20", "+", url)
  return(url)
}

check_balanced <- function(x) {
  # Check parenthesis are appropriately balanced (i.e. every open is closed)
  sapply(x, function(str) {
    str <- gsub("[^\\(\\)]", "", str)
    str <- strsplit(str, "")[[1]]
    str <- ifelse(str == "(", 1, -1)
    all(cumsum(str) >= 0) && sum(str) == 0
  })
}

show_warnings <- function(warnings){
  if (length(warnings) > 0) {
    for (w in warnings) {
      warning(w$message, call. = FALSE)
    }
  }
}

get_corpus_n <- function(corpus, default = "en-2019"){
  stopifnot(is.character(corpus))
  n <-  corpuses[corpus, "Number"]
  if (any(is.na(n)) && !is.na(default)) {
    if (is.character(default)) default <- get_corpus_n(default)
    stopifnot(default %in% corpuses$Number)
    invalid <- paste(corpus[is.na(n)], collapse = ", ")
    warning(paste0("Unknown corpus ", invalid, ". Using default corpus instead."), call. = FALSE)
    n[is.na(n)] <- default
  }
  return(n)
}

get_corpus_text <- function(n, default = NA){
  stopifnot(is.numeric(n))
  text <- row.names(corpuses)[match(n, corpuses$Number)]
  if (any(is.na(text)) && !is.na(default)) {
    if (is.numeric(default)) default <- get_corpus_text(default)
    stopifnot(default %in% row.names(corpuses))
    invalid <- paste(n[is.na(text)], collapse = ", ")
    warning(paste0("Unknown corpus ", invalid, ". Using default corpus instead."), call. = FALSE)
    text[is.na(text)] <- default
  }
  return(text)
}

truncate_years <- function(ngram){
  stopifnot(class(ngram)[1] == "ngram")
  ngram$Corpus <- as.character(ngram$Corpus)
  ngram <- left_join(ngram, select(corpuses,
                                   "Shorthand",
                                   "Last.Year"),
                     by = c("Corpus" = "Shorthand"))
  ngram <- filter(ngram, .data$Year <= "Last.Year")
  ngram$Last.Year <- NULL
  return(ngram)
}
