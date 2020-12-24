#' UnlistVector
#'
#' Turns table with single column to into vector in one step
#'
#' @param x Input object (must be table object with only one column)
#' @return Vector object
#' @export
#'


UnlistVector <- function(x){

  result <- x %>% unlist %>% as.vector
  return(result)

}
