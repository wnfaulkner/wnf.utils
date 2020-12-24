#' MostRecentlyModifiedFilename
#'
#' Find most recently modified file in a directory
#'
#' @param title.string.match Character string to be founding within the filename.
#' @param file.type File type extension.
#' @param dir Directory to search. This function only operates on a single directory and does not search subdirectories.
#' @return Character string object for most recently modified file name
#' @importFrom magrittr "%>%"
#' @export


MostRecentlyModifiedFilename <- function(
  title.string.match,
  file.type,
  dir
){
  setwd(dir)
  print(paste("File Directory: ", dir, sep = ""))
  match.files.v <-
    list.files()[
      grepl(tolower(title.string.match), tolower(list.files())) &  #match title string
        grepl(  #match file type
          tolower(file.type),
          sapply(
            tolower(list.files()),
            function(x){substr(x, nchar(x)-nchar(file.type)+1, nchar(x))}
          )
        ) &
        !grepl("\\~\\$", list.files())  #restrict to non-temporary files
      ]

  if(length(match.files.v)==0){
    stop(
      paste0(
        "No matching files. Files in directory: ",
        paste0(list.files(), collapse = ", ")
      )
    )
  }

  most.recent.match.file <-
    match.files.v[
      file.info(match.files.v)$mtime ==
      sapply(match.files.v, function(x){file.info(x)$mtime}) %>% max
    ]
  print(paste("File Name: ", most.recent.match.file, sep = ""))

  return(most.recent.match.file)
}
