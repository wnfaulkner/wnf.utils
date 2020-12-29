#' MoveColsLeft
#'
#' Move named columns to the front/left side of table
#' @param tb Table in which column(s) will be moved.
#' @param varnames Names of variables to be moved.
#' @return Tibble object
#' @export
 
    MoveColsLeft <- function(
      tb,
      varnames
    ){

      if(any(!(varnames %in% names(tb)))){
        stop(paste0("varnames '", varnames[!(varnames %in% names(tb))], "' missing from table names."))
      }

      result <-
        cbind(
          tb %>% dplyr::select(varnames),
          tb[,!(names(tb) %in% varnames)]
        ) %>% as_tibble()

      return(result)
    }
