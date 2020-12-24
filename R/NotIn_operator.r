 #' %!in%
 #'
 #' New operator that complements %in%. Whereas a %in% b returns the elements of a that are also in b, a %!in% b returns the elements of a that are NOT in b (a subtracting the intersection of a+b). 
 #' @export
 
  #New Operator - 'not in'
    '%!in%' <- function(x,y)!('%in%'(x,y))