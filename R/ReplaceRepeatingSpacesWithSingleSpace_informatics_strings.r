#' ReplaceRepeatingSpacesWithSingleSpace
#'
#' Reduce repeating substring with single occurrence of that substring
#'
#' @param x Character string in which repeated spaces are to be replaced with a single space
#' @return x with no repeated spaces
#' @importFrom tidyr tibble
#' @export

ReplaceRepeatingSpacesWithSingleSpace <-
  function(
    x
  ){
    result <- gsub('([ ])\\1+', '\\1', x)

    return(result)
  }
