#' @export

clear_eventlog <- function(file = getOption("shinyEventLogger.file")
                           ) {

  cat("",
      file = file,
      append = FALSE
      )

} # end of clear_eventlog
