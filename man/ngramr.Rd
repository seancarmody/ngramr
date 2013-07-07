\docType{package}
\name{ngramr}
\alias{ngramr}
\alias{ngramr-package}
\title{Dig into the Google Ngram Viewer using R}
\description{
  The \href{http://books.google.com/ngrams}{Google Books
  Ngram Viewer} allows you to enter a list of phrases and
  then displays a graph showing how often the phrases have
  occurred in a corpus of books (e.g., "British English",
  "English Fiction", "French") over time. The underlying
  data is hidden in web page, embedded in some Javascript.
}
\details{
  This package extracts the data an provides it in the form
  of an R dataframe.

  The key function is \code{ngram} which, given a
  collection of phrases, returns a dataframe containing the
  frequencies by year.

  Like the Google Ngram Viewer website itself, this package
  is aimed at for quick inquiries into the usage of small
  sets of phrases.

  Please respect the terms of service of the Google Books
  Ngram Viewer while using this code. This code is meant to
  help viewers retrieve data behind a few queries, not bang
  at Google's servers with thousands of queries. The
  complete dataset can be
  \href{http://storage.googleapis.com/books/ngrams/books/datasetsv2.html}{freely
  downloaded}.
}
\references{
  Based on the \code{getNgrams.py} script available on
  \href{http://www.culturomics.org/Resources/get-ngrams}{Culturomics}
  written by Jean-Baptiste Michel.
}

