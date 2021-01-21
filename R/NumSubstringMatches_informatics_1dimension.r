#' NumSubstringMatches
#'
#' Number of times specified substring occurs within a character string
#'
#' @param substring Substring pattern to be found within the string
#' @param string String to be searched
#' @return Numeric object with number of matches for 'substring' within 'string'
#' @export

NumSubstringMatches <-
  function(
    substring,
    string
  ){

    matches <-
      gregexpr(substring, as.character(string)) %>%
      unlist

    if(length(matches) == 1 && matches == -1){result <- 0}else{result <- length(matches)} #gregexpr returns -1 when there's no match

    return(result)
  }
