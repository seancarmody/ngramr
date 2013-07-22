#' Google Ngram theme for ggplot2
#' 
#' @param ... additional parameters to pass to \code{theme}
#' 
#' @details
#' Use a Google Ngram-style plot theme.
#' 
#' @export

theme_google <- function(...) {
  theme(panel.border = element_rect(colour="grey", size=0.2, fill=NA),
        panel.background = element_rect(fill = NA), 
        axis.line = element_line(colour="black", size=0.3),
        panel.grid.major=element_line(colour="grey", size=0.2),
        panel.grid.minor=element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box="vertical",
        legend.key = element_rect(fill=NA),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_blank(), ...) 
}

scale_colour_google <- function(...){
  palette = c("#264EC0", "#D22310", "#FC8608", "#168713", "#850086", "#1086B9", 
                "#D22B63", "#559D05", "#A71B23", "#21436F", "#852D86", "#219B86")
  scale_colour_manual("", values = palette, ...)
}

scale_fill_google <- function(...){
  palette = c("#264EC0", "#D22310", "#FC8608", "#168713", "#850086", "#1086B9", 
                "#D22B63", "#559D05", "#A71B23", "#21436F", "#852D86", "#219B86")
  scale_fill_manual("", values = palette, ...)
}