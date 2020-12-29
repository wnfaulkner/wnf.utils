#' OrderTbByVar
#'
#' Order table by specific variable using the variable name
#' @param tb 
#' @param order.by.varname Character string indicating name of column by which table is to be ordered
#' @return Ordered tibble
#' @export

    OrderTbByVar <-
      function(
        tb,
        order.by.varname
      ){

        result <- tb[rev(order(tb[,names(tb) == order.by.varname])),] %>% as_tibble()

        return(result)
      }
