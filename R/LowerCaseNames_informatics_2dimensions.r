#' LowerCaseNames
#'
#' Lower-case names of a table.
#' @param tb Input table
#' @return Table with names in lower-case
#' @export
 
    LowerCaseNames <- function(tb){
      names(tb) <- tolower(names(tb))
      return(tb)
    }
