#' Decode strings containing escape-encoding Unicode 

udecode <- function(string){
  uconv <- function(chars) intToUtf8(strtoi(chars, 16L))
  ufilter <- function(string) {
    if (substr(string, 1, 1)=="|") uconv(substr(string, 2, 5)) else string
  }
  string <- gsub("\\\\u(....)", ",|\\1,", string, perl=TRUE)
  strings <- unlist(strsplit(string, ","))
  string <- paste(sapply(strings, ufilter), collapse='')
  return(string)
}