 #' GoogleSheetLoadAllWorksheets
 #'	Load all worksheets from a Google Sheet Object
 #' Returns TRUE if expression throws an error, FALSE otherwise.
 #' @param url Url of a Google Sheet
 #' @export

GoogleSheetLoadAllWorksheets <- function(url) {

  gs <- gs4_get(url)
  ws_num <- gs$sheets %>% nrow
  ws_names <- gs$sheets %>% select(name) %>% UnlistVector()

  result.ls <- list()

  for(i in 1:ws_num){
    result.ls[[i]] <-
      googlesheets4::read_sheet(
        gs,
        sheet =i
      )
  }

  names(result.ls) <- ws_names

  return(result.ls)

}
