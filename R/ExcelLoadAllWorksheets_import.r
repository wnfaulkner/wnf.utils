 #' ExcelLoadAllWorksheets
 #'
 #' Load all worksheets from an Excel File. Each sheet becomes an R object with the name of the sheet.
 #' @param filename Name of Excel file to be imported
 #' @param tibble If TRUE (default) returns tibbles, if FALSE, returns data frames.
 #' @return Set of tibbles or data frames with same names as the tab names in the imported Excel file.
 #' @export
 
  ExcelLoadAllWorksheets <- function(filename, tibble = TRUE){
      sheets <- readxl::excel_sheets(filename)
      x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
      if(!tibble) x <- lapply(x, as.data.frame)
      names(x) <- sheets
      x
    }