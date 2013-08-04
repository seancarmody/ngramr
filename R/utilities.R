try_require <- function(package) {
# Thanks to Hadley for this: https://github.com/seancarmody/ggplot2
  available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts=FALSE)))
  missing <- package[!available]
  
  if (length(missing) > 0) 
    stop(paste(missing, collapse=", "), " package required for this functionality.  Please install and try again.", call. = FALSE)
}