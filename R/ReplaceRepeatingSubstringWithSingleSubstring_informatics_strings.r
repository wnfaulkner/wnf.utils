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