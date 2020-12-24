#' NumSubstringMatches
#'
#' Number of times specified substring occurs within vector of character strings
#'
#' @param pattern Pattern to be found as a substring with elements of the vector
#' @param vector Vector to be searched
#' @return Numeric object with number of matches for 'pattern' within elements of 'vector'
#' @export

NumSubstringMatches <- function(pattern, vector){
  match.vector <-
    sapply(
      gregexpr( pattern, as.character(vector)),
      function(x) if( x[1]==-1 ){ 0 }else{ length(x) }
    )
  num.substring.matches <- sum(match.vector)
  return(num.substring.matches)
}