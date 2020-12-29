#' UniqueValsFromVarnames
#'
#' Create a list of unique values from named variables
#' @param tb Table input with named variables.
#' @param varnames vector of variable names from which unique values are to be extracted.
#' @return List with vector elements named with original column names.
#' @export

    UniqueValsFromVarnames <-
      function(
        tb,
        varnames
      ){

      if(!any(varnames %in% names(tb))){
        stop(
          paste0(
            c(
              "Varnames: '",
              varnames,
              "' do not exist in table names. Table names: '",
              paste0(names(tb), collapse = ", "),
              "'."
            ),
            collapse = ""
          )
        )
      }

      result <-
        tb[,names(tb) %in% varnames] %>%
        as.data.frame %>%
        lapply(., unique) %>%
        lapply(., RemoveNA) %>%
        lapply(., as.character) %>%
        lapply(., function(x) {strsplit(x, ",")}) %>%
        lapply(., unlist) %>%
        lapply(., unique)
      return(result)
    }
