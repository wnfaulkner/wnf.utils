#' SelectVarsByName
#'
#' Select variables based on names in/not in a string vector
#' @param tb Input table
#' @param condition Either "IN" or "NOT.IN"
#' @param char.vector Character vector of names to be selected or excluded.
#' @return Table
#' @export

    SelectVarsByName <- function(tb, condition = c("IN","NOT.IN"), char.vector){
      condition <- match.arg(condition)
      if(condition == "IN"){return(tb[,names(tb) %in% char.vector])}
      if(condition == "NOT.IN"){return(tb[,!(names(tb) %in% char.vector)])}
    }