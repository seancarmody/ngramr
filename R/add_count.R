#' Add count
#' 
#' Use 1-gram count totals to convert frequencies into counts.
#' Note that this is inaccurate for n-grams for n > 1.

add_count <- function(ng) {
  ng <- merge(ng, corpus_totals[,1:3])
  ng$Count <- ng$Frequency * ng$N.1grams
  ng$N.1grams <- NULL
  return(ng)
}