#' Plot n-gram frequencies
#'
#' \code{ggram} downloads data from the Google Ngram Viewer website and
#' plots it in \code{ggplot2} style.
#'
#' @param phrases vector of phrases. Alternatively, phrases can be an ngram
#'   object returned by \code{\link{ngram}} or \code{\link{ngrami}}.
#' @param ignore_case logical, indicating whether the frequencies are case insensitive.
#'   Default is \code{FALSE}.
#' @param code_corpus logical, indicating whether to use abbreviated corpus codes
#'   or longer form descriptions. Default is \code{FALSE}.
#' @param geom the ggplot2 geom used to plot the data; defaults to "line"
#' @param geom_options list of additional parameters passed to the ggplot2 geom.
#' @param lab y-axis label. Defaults to "Frequency".
#' @param google_theme use a Google Ngram-style plot theme.
#' @param ... additional parameters passed to \code{ngram}
#' @details 
#'  Google generated two datasets drawn from digitised books in the Google
#'  books collection. One was generated in July 2009, the second in July 2012.
#'  Google will update these datasets as book scanning continues.
#'  
#' @examples 
#' ggram(c("hacker", "programmer"), year_start = 1950)
#' 
#' # Changing the geom.
#' ggram(c("cancer", "fumer", "cigarette"),
#'       year_start = 1900,
#'       corpus = "fre_2012", 
#'       geom = "step")
#'      
#' # Passing more options.
#' require(ggplot2)
#' ggram(c("cancer", "smoking", "tobacco"),
#'       year_start = 1900, 
#'       corpus = "eng_fiction_2012", 
#'       geom = "point", 
#'       geom_options = list(alpha = .5)) + 
#'   stat_smooth(method="loess", se = FALSE)
#'  
#' # Setting the layers manually.
#' ggram(c("cancer", "smoking", "tobacco"),
#'       year_start = 1900, 
#'       corpus = "eng_fiction_2012", 
#'       geom = NULL) +
#'   stat_smooth(method="loess", se=FALSE, span = 0.3)
#'  
#' # Setting the legend placement on a long query and using the Google theme.
#' # Example taken from a post by Ben Zimmer at Language Log.
#' require(ggplot2)
#' p <- c("((The United States is + The United States has) / The United States)",
#'       "((The United States are + The United States have) / The United States)")
#' ggram(p, year_start = 1800, google_theme = TRUE) +
#'       theme(legend.direction="vertical")
#'       
#' # Pass ngram data rather than phrases
#' hacker
#' ggram(hacker) + facet_wrap(~ Corpus)

#' @export

ggram <- function(phrases, ignore_case = FALSE, code_corpus = FALSE, geom = "line",
                  geom_options = list(), lab = NA, google_theme = FALSE, ...) {
  try_require(c("ggplot2", "scales"))
  if ("ngram" %in% class(phrases)) {
    ng <- phrases
  } else {
    ng <- if(ignore_case) ngrami(phrases, ...) else ngram(phrases, ...)
  }
  if (is.character(geom) && !(geom %in% c("area", "line")) && attr(ng, "smoothing") > 0) {
    warning("ngram data is smoothed. Consider setting smoothing = 0.")
  }
  ng <- within(ng, Year <- as.Date(paste(Year, 1, 1, sep="-")))
  if (!code_corpus) ng <- within(ng,
                                 levels(Corpus) <- corpuses[levels(Corpus), 1])
  p <- ggplot(data = ng, 
             aes_string(x = "Year", y = "Frequency", colour = "Phrase", fill="Phrase"))
  if (!(class(geom) == "character")) geom <- NULL
  if (!is.null(geom)) p <- p + do.call(stat_identity, c(geom = geom, geom_options))
  p <-  p + labs(x = NULL)
  if (google_theme) {
    # Google Ngram palette.
    p <- p +
      scale_colour_google() +
      scale_fill_google() +
      theme_google() + labs(y = NULL) +
      scale_x_date(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), labels = percent)
  } else {
    p <- p +
      scale_colour_discrete("") +
      scale_fill_discrete("") +
      scale_y_continuous(labels = percent)
  }
  if (!is.na(lab)) p <- p + labs(y=lab)
  return(p)
}

