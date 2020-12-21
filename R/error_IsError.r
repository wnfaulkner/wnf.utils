 #' IsError
 #'
 #' Returns TRUE if expression throws an error, FALSE otherwise.
 #' @param .expr Expression to be evaluated
 #' @return Logical
 #' @export


	IsError <- function(.expr){
	  result <-
		ifelse(
		  tryCatch(
			.expr,
			error = function(x) {return(TRUE)}
		  ) == TRUE,
		  TRUE,
		  FALSE
		)
	  return(result)
	}
