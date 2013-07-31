\name{print.ngram}
\alias{print.ngram}
\title{Print n-gram contents}
\usage{
  print.ngram(ngram, rows = 6)
}
\arguments{
  \item{ngram}{ngram object as returned by
  \code{link{ngram}}}

  \item{rows}{number of rows to print from top and bottom
  of ngram data set. Default is 6.}
}
\description{
  Print n-gram contents
}
\examples{
x <- ngram(c("hacker", "programmer"), year_start = 1950)
x
print(x, rows = 3)
}

