#' SubNA
#'
#' Replace NAs in a vector with a replacement value
#' @param x Vector input. If no NAs in the input, function will return original input.
#' @param na.replacement Value with which NAs will be substituted. 
#' @return Vector
#' @export
 
    SubNA <- function(x,na.replacement){
      x[is.na(x)] <- na.replacement
      return(x)
    }
