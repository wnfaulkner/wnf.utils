#' CreateDummyVarsFromMultChoiceMultAnsResponseStrings
#'
#' Create dummy variables from single variable with concatenated strings containing responses to multiple-choice, multiple answer columns
#' @param tb Input table
#' @param varname Name of variable with multiple-choice multiple-answer response strings
#' @param answer.options Full list of unique answer options as a character vector (sometimes there will be an answer option that no respondent selected)
#' @param output.format "logical" for TRUE/FALSE, "binary" for 1/0 (default is 'logical')
#' @return Original table containing input table and new variables.
#' @export

#

    #Test Inputs
      #tb <- create.numeric.tb
      #varname <- names(tb)[i]
      #answer.options <- ans.opt.tb %>% filter(ans.opt.set.id == ans.set.id.i) %>% select(ans.opt.char) %>% unlist %>% as.vector

    CreateDummyVarsFromMultChoiceMultAnsResponseStrings <-
      function(
        tb,
        varname,
        answer.options,
        output.format = c("logical", "binary")
      ){
        if(!is.data.frame(tb)){stop("Input 'tb' is not a data frame.")}
        if(class(varname) != "character" || length(varname) > 1){stop("Input 'varname' is either not of class character or has a length greater than 1.")}
        if(length(answer.options) == 1){warning("Only one answer option specified")}
        if(!varname %in% names(tb)){stop("Input 'varname' has no match among column names of input 'tb'.")}
        if(missing(output.format)){
          output.format <- "logical"
          warning("Output format not specified - defaulting to 'logical'.")
        }

        var <- tb %>% select(varname)

        var.tb <- #create table with TRUE/FALSE columns for each answer option by searching substrings in original variable
          sapply(
            answer.options,
            function(x){
              grepl(x, unlist(var))
            }
          ) %>% as_tibble()

        if(apply(var.tb, 2, any) %>% unlist %>% as.vector %>% any){
          warning("Searching for answer options in variable returned no results. Result is all blank columns.")
        }

        if(output.format == "binary"){
          var.tb %<>%
            apply(
              .,
              2,
              function(x){dplyr::if_else(x, 1, 0)}
            )
        }

        result <- cbind(tb, var.tb)

      }