#' Dig into the Google Ngram Viewer using R
#' 
#' The key function is \code{ngram} which, given a collection of 
#' phrases, returns a dataframe containing the frequencies with which
#' those phrases occur in the text of books in the Google Books corpus.
#' 
#' PLEASE do respect the terms of service of the Google Books Ngram Viewer
#' while using this code. This code is meant to help viewers retrieve data
#' behind a few queries, not bang at Google's  servers with thousands of queries.
#' The complete dataset can be freely downloaded here:
#' \code{http://storage.googleapis.com/books/ngrams/books/datasetsv2.html}.
#' 
#' @import RCurl httr rjson stringr reshape2
#' @docType package
#' @name ngramr
#' @aliases ngramr ngramr-package
NULL