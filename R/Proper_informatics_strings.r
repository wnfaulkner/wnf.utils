#' Proper
#'
#' Excel 'proper' function - capitalize first letter of each word in string
#'
#' @param x Character (if multiple words, words must be separated by space)
#' @return Returns x with the first letter of each word capitalized
#' @export

Proper <- function(s, strict = FALSE) {
  cap <- function(s)
    paste(
      toupper(substring(s, 1, 1)),
      {s <- substring(s, 2); if(strict) tolower(s) else s},
      sep = "", collapse = " "
    )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}