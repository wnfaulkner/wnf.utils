#' IndexMatchToVectorFromTibble
#'
#' Index-match equivalent for replacing values of a vector with values from a tibble given variable name in table
#' @param vector Vector with values to be replaced.
#' @param lookup.tb The lookup table with lookup values and corresponding replacements values
#' @param match.varname Name of variable in lookup table with values that match vector values to be replaced.
#' @param replacement.vals.varname Name of variable in the lookup table with values that will be substituted into the vector.
#' @param mult.replacements.per.cell Logical - whether there are multiple replacements in a single element of the vector.
#' @param mult.replacements.separator.char If mult.replacements.per.cell is TRUE, the character upon which elements are to be split before substitution.
#' @param print.matches Logical - whether to print a table of matches.
#' @return R object
#' @export

    #Test Inputs
      #vector = resp9.tb$pd_coaching.instruction
      #lookup.tb = config.ans.opt.tb
      #match.varname = "ans.text.agreement"
      #replacement.vals.varname = "ans.text.agreement.num"
      #mult.replacements.per.cell = TRUE
      #mult.replacements.separator.char = ","
      #print.matches = TRUE

    IndexMatchToVectorFromTibble <-
      function(
        vector,
        lookup.tb,
        match.varname,
        replacement.vals.varname,
        mult.replacements.per.cell = c(FALSE,TRUE),
        mult.replacements.separator.char = NULL,
        print.matches = c(TRUE,FALSE)
      ){

        if(mult.replacements.per.cell){
          lookup.tb <-
            SplitColReshape.ToLong(
              df = lookup.tb,
              id.varname = replacement.vals.varname,
              split.varname = match.varname,
              split.char = ","
            ) #strsplit(match.col, mult.replacements.separator.char) %>% unlist %>% as.vector
        }

        match.col <- lookup.tb %>% dplyr::select(all_of(match.varname)) %>% pull()
        replacement.col <- lookup.tb %>% dplyr::select(all_of(replacement.vals.varname)) %>% pull()
        matched.vals.ls <- list()
        unmatched.vals.ls <- list()

        for(i in 1:length(vector)){
          if(is.na(vector[i])){next()} #Skips NAs
          if(!any(match.col == vector[i])){
            unmatched.vals.ls[[i]] <- vector[i]
            warning(
              paste("No match for '", vector[i], "' found in column '", match.varname, "'.", sep = "")
            )
          }else{
            matched.vals.ls <- vector[i]
            vector[i] <- replacement.col %>% unlist %>% .[match.col == vector[i]]
          }
        }

        if(!missing(print.matches) && print.matches){
          matched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
            paste0("Values replaced: ",.) %>% print
          unmatched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
            paste0("Values not replaced: ",.) %>% print
        }
        return(vector)
      }