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
  Possible corpora: eng_2012, eng_2009, eng_us_2012,
  eng_us_2009, eng_gb_2012, eng_gb_2009, chi_sim_2012,
  chi_sim_2009, fre_2012, fre_2009, ger_2012, ger_2009,
  spa_2012, spa_2009, rus_2012, rus_2009, heb_2012,
  heb_2009, ita_2012, eng_fiction_2012, eng_fiction_2009,
  eng_1m_2009
}

