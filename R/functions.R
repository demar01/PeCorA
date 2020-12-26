#' splt_last
#'
#' @param x character of names
#' @examples
#' if (interactive()) {
#'   lapply(FUN = splt_last, paste0(list("apple", "pear"), "_1"))
#' }
#' @return list with number followed by underscore
#' @export
splt_last <- function(x) {
  tail(unlist(strsplit(x, "_")), n = 1)
}
