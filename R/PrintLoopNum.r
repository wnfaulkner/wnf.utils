 #' PrintLoopNum
 #'
 #' When placed inside a loop, will print message like the following: "LOOP 'i' -- Loop num: 7"
 #' @param loop.index Name of object using as the loop index
 #' @export
 
 PrintLoopNum <- function(loop.index){ #loop.index = object which defines loop number (e.g. 'i' in most common cases)
      print(
        paste0(
          "LOOP '",loop.index, "' -- Loop num: ", eval(parse(text = loop.index))
        )
      )
    }