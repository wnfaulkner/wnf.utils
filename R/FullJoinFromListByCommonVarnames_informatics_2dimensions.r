#' FullJoinFromListByCommonVarnames
#'
#' Full Join tibbles in a list by common column names.
#' @param list List object with multiple elements that have some common variable name(s)
#' @return Tibble
#' @export

    FullJoinFromListByCommonVarnames <-
      function(
        list
      ){
        #i = 1
        for(i in 1:(length(list)-1)){
          list.i.base <- list[[i]]
          list.i <- list[[i+1]]

          #if(intersect(names(list.i.base)) ) #! Make stop error for when no common column name

          list[[i+1]] <-
            full_join(
              list.i.base,
              list.i,
              by = names(list.i.base)[names(list.i.base) %in% names(list.i)]
            )
        }

        return(list[[length(list)]] %>% as_tibble)
      }
