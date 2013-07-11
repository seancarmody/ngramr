\name{ggram}
\alias{ggram}
\title{Plot n-gram frequencies}
\usage{
  ggram(phrases, corpus = "eng_2012", year_start = 1500,
    year_end = 2008, smoothing = 3, wide = FALSE)
}
\arguments{
  \item{phrases}{vector of phrases}

  \item{corpus}{Google corpus to search (see Details for
  possible values)}

  \item{year_start}{start year, default is 1500}

  \item{year_end}{end year, default is 2008}

  \item{smoothing}{smoothing paramater, default is 3}

  \item{wide}{a logical value indicating whether results
  should be returned in a "wide" format (phrases are column
  names) or not. The default is \code{FALSE}.}
}
\description{
  \code{ggram} downloads data from the Google Ngram Viewer
  website and plots it in \code{ggplot2} style.
}
\details{
  Google generated two datasets drawn from digitised books
  in the Google books collection. One was generated in July
  2009, the second in July 2012.  Google will update these
  datasets as book scanning continues.

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
\examples{
freq <- ngram(c("mouse", "rat"), year_start=1950)
head(freq)
}

