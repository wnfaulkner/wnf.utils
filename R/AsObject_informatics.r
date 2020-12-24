 #' AsObject
 #'
 #' Function to convert character string to object ('as object')
 #' @param x Expression to be evaluated
 #' @return R object
 #' @export
 
 AsObject <- function(x){return(eval(parse(text = x)))}