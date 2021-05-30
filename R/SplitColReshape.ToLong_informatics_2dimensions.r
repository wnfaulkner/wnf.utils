#' SplitColReshape.ToLong
#'
#' Reshape data from wide to long format based on splitting a column on a character
#' @param tb Input table
#' @param id.varname Name of variable in input table to be used as an id/primary key.
#' @param split.varname Name of variable to be split and reshaped. 
#' @param split.char Character designating where split variable should be split.
#' @return Tibble
#' @export

    #Test inputs
      #tb = ans.set.tb[,1:3]
      #id.varname = "ans.set.id"
      #split.varname = "ans.nums"
      #split.char = ";"

    SplitColReshape.ToLong <- function(tb, id.varname, split.varname, split.char){

      if(!is.data.frame(tb)){stop("Input not a data frame.")}

      if(!exists("split.char")){
        split.varname <- readline(prompt = "Enter the variable name that will be split and used to reshape data: ")
      }

      if(!exists("split.varname")){
        split.varname <- readline(prompt = "Enter the variable name that will be split and used to reshape data: ")
      }

      if(!exists("split.char")){
        split.char <- readline(prompt = "Enter the character(s) you would like to split your variable on: ")
      }

      if(dim(tb)[1] == 0){
        result <- tb
      }

      if(dim(tb)[1] != 0){
        id.var <- tb[,names(tb)==id.varname]
        split.var <- tb[,names(tb)==split.varname]

        result <-
          tb %>%
          mutate(
            new.split.var =
              sapply(
                split.var, function(x){strsplit(x,split.char)}
              )
          ) %>%
          unnest(new.split.var, .drop = FALSE) %>%
          .[,names(.)[names(.) != split.varname]] %>%
          OrderTbByVar(
            tb = .,
            order.by.varname = id.varname
          ) %>%
          ReplaceNames(
            tb = .,
            current.names = "new.split.var",
            new.names = split.varname
          )
      }

      return(result %>% as_tibble)
    }
