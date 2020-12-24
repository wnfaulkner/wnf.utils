 #' StopQuietly
 #'
 #' Performs the same as stop() but with no error message
 #' @export
 
 
  	StopQuietly <- function() {
		  opt <- options(show.error.messages = FALSE)
		  on.exit(options(opt))
		  stop()
		}