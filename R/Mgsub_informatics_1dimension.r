#' Mgsub
 #'
 #' 'Multiple gsub' to find/replace multiple patterns in a vector
 #' @param pattern Vector of character strings to be found and substituted.
 #' @param replacement Vector of character strings to be put in place of pattern values. Length and order matters: replacement vector must be of the same length as pattern vector. Values correspond via order of vector (first element in replacement vector will be substituted for the first element of the pattern).
 #' @param x Vector in which values are to be substituted.
 #' @return R object
 #' @export

    Mgsub <- function(
      pattern,
      replacement,
      x
    ){
      n <- length(pattern)

      if (n != length(replacement)) {
        print(pattern)
        print(replacement)
        stop("Pattern and replacement do not have the same length.")
      }

      for (i in 1:n) {
        if(x[grep(pattern[i], x)] %>% length %>% equals(0)){next()}

        x[grep(pattern[i], x)] <-
          gsub(
            pattern[i],
            replacement[i],
            x[grep(pattern[i], x)]
          )
      }

      result <- x

      return(result)
    }
