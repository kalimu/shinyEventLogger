
#' @family low-level logging functions

log_to_js_console <- function(header,
                              body = ""
                              ) {

  if (missing(header)) stop("A header of log entry is missing.")

  session <- shiny::getDefaultReactiveDomain()

  if (is.null(session)) return(FALSE)

  custom_message <-

    if (body == "") {

      header

    } else {

      paste0(header, "\n", body)

    }

  session$sendCustomMessage(type = "log_event",
                            message = custom_message)

  return(TRUE)

} # end of log_to_js_console()




