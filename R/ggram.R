#' Plot n-gram frequencies
#'
#' \code{ggram} downloads data from the Google Ngram Viewer website and
#' plots it in \code{ggplot2} style.
#'
#' @param phrases vector of phrases. Alternatively, phrases can be an ngram
#'   object returned by \code{\link{ngram}} or \code{\link{ngrami}}.
#' @param ignore_case logical, indicating whether the frequencies are case
#'  insensitive.
#'  Default is \code{FALSE}.
#' @param code_corpus logical, indicating whether to use abbreviated corpus
#'  `codes or longer form descriptions. Default is \code{FALSE}.
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
#' \donttest{library(ggplot2)
#' ggram(c("hacker", "programmer"), year_start = 1950)
#'
#' # Changing the geom.
#' ggram(c("cancer", "fumer", "cigarette"),
#'       year_start = 1900,
#'       corpus = "fr-2012",
#'       smoothing = 0,
#'       geom = "step")
#'
#' # Passing more options.
#' ggram(c("cancer", "smoking", "tobacco"),
#'       year_start = 1900,
#'       corpus = "en-fiction-2012",
#'       geom = "point",
#'       smoothing = 0,
#'       geom_options = list(alpha = .5)) +
#'   stat_smooth(method="loess", se = FALSE, formula = y  ~ x)
#'
#' # Setting the layers manually.
#' ggram(c("cancer", "smoking", "tobacco"),
#'       year_start = 1900,
#'       corpus = "en-fiction-2012",
#'       smoothing = 0,
#'       geom = NULL) +
#'   stat_smooth(method="loess", se=FALSE, span = 0.3, formula = y ~ x)
#'
#' # Setting the legend placement on a long query and using the Google theme.
#' # Example taken from a post by Ben Zimmer at Language Log.
#' p <- c("((The United States is + The United States has) / The United States)",
#'       "((The United States are + The United States have) / The United States)")
#' ggram(p, year_start = 1800, google_theme = TRUE) +
#'       theme(legend.direction="vertical")
#'
#' # Pass ngram data rather than phrases
#' ggram(hacker) + facet_wrap(~ Corpus)
#'}
#' @export

ggram <- function(phrases, ignore_case = FALSE, code_corpus = FALSE,
                  geom = "line", geom_options = list(), lab = NA,
                  google_theme = FALSE, ...) {
  if ("ngram" %in% class(phrases)) {
    ng <- phrases
  } else {
    if (ignore_case) {
      ng <- ngrami(phrases, ...)
    } else {
      ng <- ngram(phrases, ...)
    }
  }
  if (is.null(ng)) {
    message("Unable to plot: no data returned")
    return(invisible(NULL))
  }
  if (is.character(geom) &&
      !(geom %in% c("area", "line")) && attr(ng, "smoothing") > 0) {
    warning("ngram data is smoothed. Consider setting smoothing = 0.")
  }
  if (!"Year" %in% names(ng)) stop("No ngram data returned")
  ng <- within(ng, Year <- as.Date(paste(Year, 1, 1, sep = "-")))
  if (!code_corpus) ng <- within(ng,
                                 levels(Corpus) <- corpuses[levels(Corpus),
                                                            "Informal.corpus.name"])
  p <- ggplot(data = ng,
             aes_string(x = "Year", y = "Frequency",
                        colour = "Phrase", fill = "Phrase",
                        label = "Phrase"))
  if (!inherits(geom, "character")) geom <- NULL
  if (!is.null(geom)) p <- p + do.call(stat_identity,
                                       c(geom = geom, geom_options))
  p <-  p + labs(x = NULL)
  if (google_theme) {
    # Google Ngram palette.
    p <- p +
      scale_colour_google() +
      scale_fill_google() +
      theme_google() + labs(y = NULL, colour = NULL) +
      scale_x_date(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), labels = scales::percent)
  } else {
    p <- p +
      scale_colour_discrete("") +
      scale_fill_discrete("") +
      scale_y_continuous(labels = scales::percent)
  }
  if (!is.na(lab)) p <- p + labs(y = lab)
  return(p)
}
