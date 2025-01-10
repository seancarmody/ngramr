#' ngramr: Dig into the Google Ngram Viewer using R
#'
#' @description
#' The \href{http://books.google.com/ngrams}{Google Books Ngram Viewer}
#' allows you to enter a list of phrases and then displays a graph showing
#' how often the phrases have occurred in a corpus of books
#' (e.g., "British English", "English Fiction", "French") over time.
#' The underlying data is hidden in web page, embedded in some Javascript.
#'
#' This package extracts the data an provides it in the form of an R dataframe.
#'
#' The key function is \code{ngram} which, given a collection of
#' phrases, returns a dataframe containing the frequencies by year.
#'
#' The code is based on the \code{getNgrams.py} Python script available on
#' \href{https://web.archive.org/web/20221129120802/https://www.culturomics.org/}{Culturomics Code}
#' written by Jean-Baptiste Michel. The Culturomics website doesn't 
#' exist anymore but can still be find 
#' \href{https://web.archive.org/web/20221129220150/https://www.culturomics.org/Resources/get-ngrams}{on archive.org}
#' and is worth exploring.
#'
#' Note that compared to the 2009 versions, the 2012 and 2019 versions have
#' larger numbers of books, improved OCR, improved library and publisher
#' metadata. The 2012 and 2019 corpuses also don't form ngrams that cross
#' sentence boundaries, and do form ngrams across page boundaries and 
#' support part of speech tagging, unlike the 2009 versions.
#'
#' Like the Google Ngram Viewer website itself, this package is aimed at for
#' quick inquiries into the usage of small sets of phrases.
#'
#' Please respect the terms of service of the Google Books Ngram Viewer while
#' using this code. This code is meant to help viewers retrieve data behind
#' a few queries, not bang at Google's  servers with dozens of queries.
#' The complete dataset can be
#' \href{https://storage.googleapis.com/books/ngrams/books/datasetsv3.html}{downloaded here}.
#'
#' @references
#' Michel, Jean-Baptiste, et al. "Quantitative analysis of culture using
#' millions of digitized books." \emph{Science} 331, No. 6014 (2011): 176--182.
#' 
#' @keywords internal
#' @import dplyr tidyr ggplot2 
#' @importFrom rlang .data 
#' @docType package
#' @name ngramr
#' @aliases ngramr ngramr-package
"_PACKAGE"
