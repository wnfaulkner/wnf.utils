 #' ListToTibbleObjects
 #'
 #' Converts all elements in a list to separate R objects (tibbles). Uses names of list elements to assign names to R Objects.
 #' @param list
 #' @return Set of tibbles with contents of each list element.
 #' @export

    ListToTibbleObjects <- function(list){
      for(i in 1:length(list)){

        object.name.i <- paste(names(list)[i], ".tb", sep = "")

        assign(
          object.name.i,
          list[[i]],
          pos = 1
        )

        print(paste(i, ": ", object.name.i, sep = ""))
      }
    }