#' ReplaceRepeatingSubstringWithSingleSubstring
#'
#' Reduce repeating substring with single occurrence of that substring
#'
#' @param substring Repeated character pattern to be replaced with a single version of itself within 'string'
#' @param string Character string in which substrings are to be replaced
#' @return 'string' with no repeated substrings
#' @importFrom tidyr tibble
#' @export

ReplaceRepeatingSubstringWithSingleSubstring <-
  function(
    substring,
    string
  ){

    max.num.replace <- NumSubstringMatches(substring, string)

    if(max.num.replace == 0){

      warning("No occurrences of 'substring' found in 'string'. Returning original vector.")
      result <- string

    }else{

      replacement.tb <-
        tibble(
          pattern = strrep(substring, 1:max.num.replace) %>% rev,
          replacement = rep(" ", max.num.replace)
        ) %>%
        filter(pattern != replacement)

      for(i in 1:nrow(replacement.tb)){
        string <-
          gsub(
            replacement.tb$pattern[i],
            replacement.tb$replacement[i],
            string
          )
      }

      result <- string
        #Mgsub( #THROWS ERROR BECAUSE THIS IS MY OWN FUNCTION!!!!!!!!!!!!!!!!!!!!!!
        #  pattern = replacement.tb$pattern,
        #  replacement = replacement.tb$replacement,
        #  x = .
        #)
    }

    return(result)
  }
