#' AsObject
#'
#' Output variable names in data frame which are of user-defined class
#' @param tb Table (data frame or tibble) whose names will be returned.
#' @param colclass Class of columns whose names will be returned. Must be one of the following: "numeric","character","logical","factor","date","integer".
#' @return Character vector of names from tb matching colcass argument.
#' @export

 VarnamesOfClass <- 
	 function(
       tb,
       colclass = c("numeric","character","logical","factor","date","integer") 
     ){
        colclass <- match.arg(colclass)
        
        name.classes <- 
          lapply(tb, class) %>% 
          unlist %>% 
          as.vector(.)
        
        result <- 
          names(tb)[which(name.classes %in% colclass)]
        
        return(result)
      }
        