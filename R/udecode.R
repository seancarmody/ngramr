#' Decode strings containing escape-encoding Unicode 
#' 
#' @param string a character string which may include escaped Unicode characters

udecode <- function(string){
  uconv <- function(chars) intToUtf8(strtoi(chars, 16L))
  ufilter <- function(string) {
    if (substr(string, 1, 1)=="|") uconv(substr(string, 2, 5)) else string
  }
  string <- gsub("\\\\u([[:xdigit:]]{4})", ",|\\1,", string, perl=TRUE)
  strings <- unlist(strsplit(string, ","))
  string <- paste(sapply(strings, ufilter), collapse='')
  return(string)
}