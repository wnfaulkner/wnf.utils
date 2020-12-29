#' TableWithNA
#'
#' Shortcut to use base R table() function and return number of NAs.
#' @param x Vector or variable input.
#' @return R Table
#' @export

    TableWithNA <- function(x){
      result <-
        table(x, useNA = "always")
      return(result)
    }