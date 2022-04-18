#' RemoveAllBlankNAColumns
#'
#' Select variables based on names in/not in a string vector
#' @param tb Input table
#' @return Table without columns where all rows were blank or NA
#' @export

    RemoveAllBlankNAColumns <- function(tb){
		
		nrow.tb <- tb %>% nrow
		
		var.select.vector <- 
			tb %>%
			apply(., c(1,2), function(x){x==""|is.na(x)}) %>%
			colSums %>%
			is_less_than(nrow.tb) %>% 
			as.vector
			
		return(tb[,var.select.vector])
    }