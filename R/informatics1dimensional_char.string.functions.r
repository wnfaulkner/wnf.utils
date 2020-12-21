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

#' NumSubstringMatches
#'
#' Number of times specified substring occurs within vector of character strings
#'
#' @param pattern Pattern to be found as a substring with elements of the vector
#' @param vector Vector to be searched
#' @return Numeric object with number of matches for 'pattern' within elements of 'vector'
#' @export

NumSubstringMatches <- function(pattern, vector){
  match.vector <-
    sapply(
      gregexpr( pattern, as.character(vector)),
      function(x) if( x[1]==-1 ){ 0 }else{ length(x) }
    )
  num.substring.matches <- sum(match.vector)
  return(num.substring.matches)
}

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


#' ReplaceRepeatingSubstringWithSingleSubstring
#'
#' Reduce repeating substring with single occurrence of that substring
#'
#' @param string Character string in which substrings are to be replaced
#' @param substring Repeated character pattern to be replaced with a single version of itself within 'string'
#' @return 'string' with no repeated substrings
#' @importFrom tidyr tibble
#' @export

ReplaceRepeatingSubstringWithSingleSubstring <-
  function(
    string,
    substring
  ){
    max.num.replace <- NumSubstringMatches(substring, string) %>% max
    replacement.tb <-
      tibble(
        pattern = strrep(substring, 1:max.num.replace) %>% rev,
        replacement = rep(" ", max.num.replace)
      )

    string %>%
      mgsub( #THROWS ERROR BECAUSE THIS IS MY OWN FUNCTION!!!!!!!!!!!!!!!!!!!!!!
        pattern = replacement.tb$pattern,
        replacement = replacement.tb$replacement,
        .
      )
  }

