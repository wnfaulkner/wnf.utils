 #' GoogleSheetLoadAllWorksheets
 #'	Load all worksheets from a Google Sheet Object
 #' Returns TRUE if expression throws an error, FALSE otherwise.
 #' @param gs Google Sheet object
 #' @export
 
 GoogleSheetLoadAllWorksheets <- function(gs) {

  ws_num <- gs$n_ws
  ws_names <- gs$ws %>% select(ws_title) %>% unlist

  result.ls <- list()

  for(i in 1:ws_num){
	result.ls[[i]] <-
	  gs_read(
		gs,
		ws = ws_names[i],
		range = NULL,
		literal = TRUE
	  )

	names(result.ls[[i]]) <- names(result.ls[[i]]) %>% tolower

  }

  names(result.ls) <- ws_names

  return(result.ls)

}