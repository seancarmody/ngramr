\name{ngram}
\alias{ngram}
\title{Get n-gram frequencies}
\usage{
  ngram(phrases, corpus = "eng_2012", year_start = 1500,
    year_end = 2008, smoothing = 3)
}
\arguments{
  \item{phrases}{vector of phrases}

  \item{corpus}{Google corpus to search}

  \item{year_start}{start year, default is 1500}

  \item{year_end}{end year, default is 2008}

  \item{smoothing}{smoothing paramater, default is 3}
}
\description{
  Get n-gram frequencies
}
\details{
  Possible corpora: \code{eng_2012}, \code{eng_2009},
  \code{eng_us_2012}, \code{eng_us_2009},
  \code{eng_gb_2012}, \code{eng_gb_2009},
  \code{chi_sim_2012}, \code{chi_sim_2009},
  \code{fre_2012}, \code{fre_2009}, \code{ger_2012},
  \code{ger_2009}, \code{spa_2012}, \code{spa_2009},
  \code{rus_2012}, \code{rus_2009}, \code{heb_2012},
  \code{heb_2009}, \code{ita_2012},
  \code{eng_fiction_2012}, \code{eng_fiction_2009},
  \code{eng_1m_2009}
}

