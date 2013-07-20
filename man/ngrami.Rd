\name{ngrami}
\alias{ngrami}
\title{Get n-gram frequencies (case insensitive version)}
\usage{
  ngrami(phrases, aggregate = TRUE, wide = FALSE, ...)
}
\arguments{
  \item{phrases}{vector of phrases}

  \item{aggregate}{sum up each of the terms}

  \item{wide}{a logical value indicating whether results
  should be returned in a "wide" format (phrases are column
  names) or not. The default is \code{FALSE}.}

  \item{...}{remaining parameters passed to ngram}
}
\description{
  Get n-gram frequencies (case insensitive version)
}

