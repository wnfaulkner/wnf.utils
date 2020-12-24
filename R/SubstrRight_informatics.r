#' SubstrRight
#'
#' Select right 'n' characters of string
#'
#' @param x Character string
#' @param n Number of characters to be selected, starting on the right end of x
#' @return Character string object with the 'n' rightmost characters of 'x'
#' @export

SubstrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}