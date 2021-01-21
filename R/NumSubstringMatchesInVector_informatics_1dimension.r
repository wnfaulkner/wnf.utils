#' NumSubstringMatchesInVector
#'
#' Number of times specified substring occurs within a character string
#'
#' @param substring Substring pattern to be found within the each element of 'vector'
#' @param vector Vector to be searched
#' @return Numeric vector object with number of matches for 'substring' within each element of 'vector'
#' @export

NumSubstringMatchesInVector <-
  function(
    substring,
    vector
  ){

    result <-
      sapply(
        vector,
        function(x){NumSubstringMatches(substring, x)}
      ) %>%
      as.vector

    return(length)
  }
