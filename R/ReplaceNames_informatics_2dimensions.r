#' ReplaceNames 
#'
#' Replace selected names the names in a data frame or tibble.
#' @param tb Table where names will be replaced. Must be data frame or tibble.
#' @param current.names Names to be replaced. Must be a character string.
#' @param new.names New names that will be inserted in the place of current.names. Must be a character string.
#' @return R data table or tibble object with different column names (replaces R object with original names)
#' @export

ReplaceNames <- function(tb,current.names, new.names) {

      #Data Checks
      if(!is.data.frame(tb)){
        stop("Input not a data frame. Input must be of class 'data.frame'.")
      }

      #New Names Checks
        if(!exists("new.names")){
          new.names <- readline(prompt = "No new names defined. Enter a vector of new names to replace current names: ")
        }

        if(!is.character(new.names)){
          new.names <- as.character(new.names)
          warning("'new.names' input not of class 'character.' Coercing to character vector.")
        }

      if(!is.character(new.names)){
        new.names <- as.character(new.names)
        warning("'new.names' input not of class 'character.' Coercing to character vector.")
      }

      #Current Names Checks
      if(!exists("current.names")){

        if(length(names(tb)) == length(new.names)){
          print("No current names to replace specified. All current names will be replaced.")
          current.names <- names(tb)
        }

        if(length(names(tb)) != length(new.names)){
          stop(
            paste(
              "No current names to replace specified. Current tb has ",
              length(names(tb)),
              " columns. New names is of length ",
              length(new.names),
              ".",
              sep = ""
            )
          )
        }

      } #End of if statement for when current.names not defined by user

      if(any(!current.names %in% names(tb))){
        warning(
          paste(
            "One or more current.names were not found in input data frame: '",
            current.names[!current.names %in% names(tb)],
            "'. ",
            sep = ""
          )
        )
      }

      #Actual Function: name replacement
      names(tb)[names(tb) %in% current.names] <- new.names
      return(tb)
    }
