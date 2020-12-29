#' TransposeTable
#'
#' Transpose table with correct column & row names.
#' @param tb Input table
#' @param keep.first.varname Logical TRUE/FALSE indicating whether to preserve the name of the first variable in the transposed table
#' @return Table
#' @export

    TransposeTable <- function(
      tb,
      keep.first.varname = c(TRUE,FALSE)
    ){

      result.rownames <- names(tb)

      result1.tb <- tb %>% t %>% as_tibble %>% mutate(x = result.rownames)

      result.varnames <- result1.tb[1,] %>% unlist %>% as.vector

      result2.tb <-
        result1.tb[-1,] %>%
        ReplaceNames(., names(.), result.varnames) %>%
        MoveColsLeft(., varnames = names(.)[ncol(.)])

      if(missing(keep.first.varname)){keep.first.varname <- FALSE}

      if(!keep.first.varname){
        names(result2.tb)[1] <- ""
      }

      return(result2.tb)
    }