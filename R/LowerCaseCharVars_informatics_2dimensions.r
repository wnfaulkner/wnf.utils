#' LowerCaseCharVars
#'
#' Lower-case all character values in a tibble
#' @param tb Input table.
#' @return Table with all character variables in lower-case.
#' @export

    LowerCaseCharVars <- function(tb){
      for(i in 1:dim(tb)[2]){
        if(class(tb[[i]]) == "character"){
          tb[[i]] <- tolower(tb[[i]])
        }
      }
      return(tb)
    }