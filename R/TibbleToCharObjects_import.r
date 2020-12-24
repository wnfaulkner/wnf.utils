#' TibbleToCharObjects 
#'
#' Assign values in column of a tibble to character string objects with name from the same row in another column.
#' @param tibble Input tibble.
#' @param object.names.colname Name of column from which names for new objects to be created will be read.
#' @param object.values.colname Name of column from which values of the new objects will be created.
#' @return Set of named R character string objects.
#' @export

  
  #Assign values in second column of a tibble to character string objects named from first column in a table
    #object names replace a space with a "." and lower case all characters
    #Test Inputs
      #tibble <- global.configs.tb
      #object.names.colname <- "config.name"
      #object.values.colname <- "config.value"

    TibbleToCharObjects <- function(tibble, object.names.colname, object.values.colname){

      if(tibble %>% names %>% equals(object.names.colname) %>% any %>% not){object.names.colname.error <- TRUE}else{object.names.colname.error <- FALSE}
      if(tibble %>% names %>% equals(object.values.colname) %>% any %>% not){object.values.colname.error <- TRUE}else{object.values.colname.error <- FALSE}
      if(object.names.colname.error & !object.values.colname.error){stop("Column name for object names does not exist in tibble.")}
      if(!object.names.colname.error & object.values.colname.error){stop("Column name for object values does not exist in tibble.")}
      if(object.names.colname.error & object.values.colname.error){stop("Neither column names specified exist in data.")}

      for(i in 1:dim(tibble)[1]){

        object.names.colnum <- names(tibble) %>% equals(object.names.colname) %>% which
        object.values.colnum <- tibble %>% names %>% equals(object.values.colname) %>% which

        object.name.i <- tibble[i,object.names.colnum] %>% unlist %>% tolower(.) %>% gsub(" ", ".", .)

        assign(
          object.name.i,
          tibble[i,object.values.colnum] %>% unlist %>% tolower,
          pos = 1
        )

        print(paste(i, ": ", object.name.i, sep = ""))
      }
    }