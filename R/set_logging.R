
#' @export

set_logging <- function(r_console = TRUE,
                        js_console = TRUE,
                        file = FALSE) {

  options('shinyEventLogger_r_console'  = FALSE)
  options('shinyEventLogger_js_console' = FALSE)
  options('shinyEventLogger_file'       = FALSE)
  options('shinyEventLogger_counter'    = 1)

  if (r_console)  {

    options('shinyEventLogger_r_console'  = TRUE)
    message("Logging to R console:          ENABLED")

  } else {

    message("Logging to R console:          DISABLED")

  }

  if (js_console) {

    options('shinyEventLogger_js_console' = TRUE)
    message("Logging to JavaScript console: ENABLED")

  } else {

    message("Logging to JavaScript console: DISABLED")

  }

  if (is.logical(file) && !file) {

    message("Logging to a file:             DISABLED")

  } else {

    log_file_name <- 'events.log'

    if (is.character(file)) {

      log_file_name <- file

    }

    if ((is.logical(file) && file) || is.character(file)) {

      options('shinyEventLogger_file' = log_file_name)
      message("Logging to the file:           ", log_file_name)

    }

  }

  if (!r_console &
      !js_console &
      (is.logical(file) && !file)
      ) {

    message("All types of logging are disabled!")

  }

} # end of set_logging()

