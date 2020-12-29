#' IsSpecialChar
#'
#' Detect Special Character and return TRUE/FALSE
#' @param char Character string input of length 1.
#' @return Logical TRUE/FALSE
#' @export
 
    IsSpecialChar <- function(char){
      if(class(char) != "character"){stop("Input must be of class 'character'.")}
      if(nchar(char) != 1){stop("Input must be a character of length 1.")}
      return(grepl("\\\\|\\^|\\$|\\.|\\?|\\*|\\||\\+|\\(|\\)|\\[|\\{",char))
    }
