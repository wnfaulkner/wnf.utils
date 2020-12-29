#' UniqueCombnFromVarnames
#'
#' All possible combinations of unique values of variables in a data frame (restricted to one value from each named column per combination).
#' @param tb Table input with named variables.
#' @param varnames vector of variable names from which combinations of unique values will be produced.
#' @return List with vector elements named with original column names.
#' @export

UniqueCombnFromVarnames <-
  function(
	tb,
	varnames
  ){
  if(!(varnames %in% names(tb))){
	stop(
	  paste0(
		c(
		  "Varnames: '",
		  varnames,
		  "' do not exist in table names. Table names: '",
		  paste0(names(tb), collapse = ", "),
		  "'."
		),
		collapse = ""
	  )
	)
  }

  result <-
	UniqueValsFromColnames(tb, varnames) %>%
	expand.grid(., stringsAsFactors = FALSE) %>%
	ReplaceNames(., current.names = names(.), new.names = varnames)
  return(result)
}

