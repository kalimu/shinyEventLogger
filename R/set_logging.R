
#' @export

set_logging <- function(r_console = TRUE,
                        js_console = TRUE,
                        file = FALSE) {

  options('shinyEventLogger.r_console'  = FALSE)
  options('shinyEventLogger.js_console' = FALSE)
  options('shinyEventLogger.file'       = FALSE)
  options('shinyEventLogger.counter'    = 1)

  log_params()

  if (!r_console &
      !js_console &
      (is.logical(file) && !file)
      ) {

    message("All types of logging are disabled!")
    return(FALSE)

  } # end of if

  # r_console ################################################################
  if (r_console)  {

    options('shinyEventLogger.r_console'  = TRUE)
    message("Logging to R console:          ENABLED")

  } else {

    message("Logging to R console:          DISABLED")

  }

  # js_console ###############################################################
  if (js_console) {

    options('shinyEventLogger.js_console' = TRUE)
    message("Logging to JavaScript console: ENABLED")

  } else {

    message("Logging to JavaScript console: DISABLED")

  }

  # file #####################################################################
  if (is.logical(file) && !file) {

    message("Logging to a file:             DISABLED")

  } else {

    if (is.logical(file) && file) {

      file  <- 'events.log'

    }

    options('shinyEventLogger.file' = file)
    message("Logging to the file:           ", file)

    if (!file.exists(file)) {

      message("File log doesn't exist.\n",
              "Your current working directory:\n",
              getwd())

      if (file.create(file)) {

        message("New log file: '", file, "' has been created.")

      } else {

        warning("Unable to create log file.",
                "Please check file path and permissions.")

        return(FALSE)

      } # end of if

    } # end of if

  } # end of if

  return(TRUE)


} # end of set_logging()

