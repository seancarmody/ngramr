add_count <- function(ng) {
  ng_attrib <- attributes(ng)[c("case_sensitive", "smoothing")]
  ng$Phrase.n <- unlist(lapply(str_split(as.character(ng$Phrase), " "), length))
  ng <- merge(ng, corpus_totals[,1:4])
  ng$Count <- round(ng$Frequency * (ng$N.1grams - ng$Pages * (ng$Phrase.n - 1)), 1)
  ng$N.1grams <- NULL
  ng$Pages <- NULL
  ng$Phrase.n <- NULL
  attributes(ng) <- c(attributes(ng), ng_attrib)
  class(ng) <- c("ngram", class(ng))
  return(ng)
}

calc_frequency <- function(ng) {
  ng_attrib <- attributes(ng)[c("case_sensitive", "smoothing")]
  ng <- merge(ng, corpus_totals[,1:3])
  ng$Frequncy <- ng$Count * ng$N.1grams
  ng$N.1grams <- NULL
  attributes(ng) <- c(attributes(ng), ng_attrib)
  class(ng) <- c("ngram", class(ng))
  return(ng)
}
