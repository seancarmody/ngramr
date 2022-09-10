#' Chunk a vector or list
#'
#' \code{chunk} takes a vector (or list) and returns a list of chunks
#' which all have lengths (approximately) equal to a specified value.
#'
#' @param x vector of list
#' @param len target length of chunks
#' @param n number of chunks
#' 
#' @details 
#'  If \code{n} is specified, \code{len} is ignored and \code{chunk} returns
#'  a list of length \code{n} of "chunks" of \code{x}. Otherwise
#'  \code{n} is calculated to break the vector into chunks which are
#'  each approximately of length \code{len}. If both \code{len} and
#'  \code{n} are unspecified, \code{chunk} simply returns \code{x}.
#' @examples
#' chunk(letters, 10)
#' chunk(LETTERS, n = 3)
#' 
#' @export

chunk <- function(x, len = NULL, n = NULL) {
  if (is.null(len) & is.null(len)) return(x)
  if (is.null(len)) len <- ceiling(length(x) / n)
  if (is.null(n)) n <- ceiling(length(x) / len)
  if (len >= length(x)) {
    return(x)
    } else {
    return(split(x, cut(seq_along(x), n, labels = FALSE)))
  }
}
