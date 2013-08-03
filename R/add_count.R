add_count <- function(ng) {
  ng <- merge(ng, corpus_totals[,1:3])
  ng$Count <- ng$Frequency * ng$N.1grams
  ng$N.1grams <- NULL
  return(ng)
}

calc_frequency <- function(ng) {
  ng <- merge(ng, corpus_totals[,1:3])
  ng$Frequncy <- ng$Count * ng$N.1grams
  ng$N.1grams <- NULL
  return(ng)
}
