 #' InstallCommonPackages
 #'
 #' Single command to install a number of commonly used packages. 
 #' Current list: 
 #' 	chron 
 #' 	devtools
 #' 	readr
 #' 	data.table
 #'		dplyr
 #'		tidyr		
 #'		googlesheets
 #'		stringr
 #'		officer
 #'		magrittr
 #'		reshape2
 #'		ggplot2
 #'		xlsx
 #'		styler
 #' @export

    InstallCommonPackages <- function(){
      install.packages('chron')
      install.packages('devtools')
      install.packages("readr")
      install.packages("data.table")
      install.packages("dplyr")
      install.packages('tidyr')
      install.packages("googlesheets")
      install.packages("stringr")
      install.packages('officer')
      install.packages("magrittr")
      install.packages('reshape2')
      install.packages('ggplot2')
      install.packages('xlsx')
      #install.packages('styler')
    }
