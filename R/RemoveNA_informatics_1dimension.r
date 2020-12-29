 #' RemoveNA
 #'
 #' Removes NA from vector
 #' @param x Vector input from which all NAs will be removed.
 #' @return Vector with no NAs.
 #' @export
 
    RemoveNA <- function(x){
      if(!is.null(dim(x))){stop("Input must be a vector.")}
      result <- x[!is.na(x)]
      return(result)
    }