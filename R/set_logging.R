#' Settings for event logging
#'
#' \code{set_logging} should be used outside ui and server functions,
#' possibly in \code{global.R}, to be used only once to define
#' where the logging should be done.
#' Events can be sent to R console, browser JavaScript console,
#' a eventlog file, or a database (or any combination of these).
#' By default logging is done to the R console and JavaScript console.
#' \code{set_logging} also can be used
#' to define global event parameters - named objects passed to \code{...}
#' that will be evaluated and added to lists of parameters of all events.
#'
#' \code{set_logging} assigns to the parent frame a new environment
#' \code{log_settings_global} for storing global event parameters.
#' If \code{database = TRUE} additional database connection object
#' named \code{log_db} is assigned to the parent frame as well.
#'
#' @param r_console A logical. Should events be logged into R console?
#'   Default is \code{TRUE}.
#' @param js_console A logical. Should events be logged into browser
#'   JavaScript console? Default is \code{TRUE}.
#' @param file A logical or a character string.
#'   Should events be logged to a file?
#'   Default is \code{FALSE}.
#'   If \code{TRUE} the default eventlog filename is \code{"events.log"}.
#'   If character string, path and name of the filelog.
#' @param database A logical or a character string.
#' Should events be logged into a database?
#'   Default is \code{FALSE}.
#'   If \code{TRUE} or \code{"mongoDB"} the connection URL to the database
#'   will be read from the first line of a text file named \code{".db_url"}.
#' @param ... a set of named objects
#'   (usually of type character, numeric, or date)
#'  to be logged as parameters common to all events.
#'
#' @importFrom mongolite mongo
#'
#' @family setting up logging parameters functions
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'
#'   set_logging(r_console = TRUE,
#'               js_console = FALSE,
#'               "param_1" = 1,
#'               "param_2" = "A")
#'
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(),
#'     server = function(input, output) {
#'       set_logging_session()
#'       log_event("Event with global params")
#'     }
#'   )
#' }

set_logging <- function(r_console  = TRUE,
                        js_console = TRUE,
                        file       = FALSE,
                        database   = FALSE,
                        ...
                        ) {

  options('shinyEventLogger.r_console'  = FALSE)
  options('shinyEventLogger.js_console' = FALSE)
  options('shinyEventLogger.file'       = FALSE)
  options('shinyEventLogger.database'   = FALSE)

  global_params <- eval(list(...))

  assign("log_settings_global",
         list2env(global_params),
         envir = parent.frame())

  if (!r_console &&
      !js_console &&
      (is.logical(file) && !file) &&
      (is.logical(database) && !database)
      ) {

    message("All types of logging are disabled!")
    return(FALSE)

  } # end of if

  # r_console ################################################################
  if (r_console)  {

    options('shinyEventLogger.r_console'  = TRUE)
    message("Logging to R console:          ENABLED")

  } else {

    message("Logging to R console:          disabled")

  }

  # js_console ###############################################################
  if (js_console) {

    options('shinyEventLogger.js_console' = TRUE)
    message("Logging to JavaScript console: ENABLED")

  } else {

    message("Logging to JavaScript console: disabled")

  }

  # file #####################################################################
  if (is.logical(file) && !file) {

    message("Logging to a file:             disabled")

  } else {

    if (is.logical(file) && file) {

      file  <- 'events.log'

    }

    options('shinyEventLogger.file' = file)

    if (!file.exists(file)) {

      message("File log doesn't exist.\n",
              "Your current working directory:\n",
              getwd())

      if (file.create(file)) {

        message("New log file: '", file, "' has been created.")

        message("Logging to the file:           ENABLED: ", file)

      } else {

        message("Logging to a file:             ERROR")

        warning("Unable to create log file.",
                "Please check file path and permissions.")

        return(FALSE)

      } # end of if

    } else {

      message("Logging to the file:           ENABLED: ", file)

    } # end of if

  } # end of if

  # database ##################################################################
  if (is.logical(database) && !database) {

    message("Logging to a database:         disabled")

  } else {

    if (is.logical(database) && database) {

      database  <- 'MongoDB'

    }

    db_url_file <- ".db_url"

    if (!file.exists(db_url_file)) {

      message("Logging to a database:         ERROR")

      message("File with URL to MongoDB doesn't exist.\n",
              "Your current working directory:\n",
              getwd())

      message("You need to create a text file named '", db_url_file, "' ",
              "in the working directory of your app and ",
              "store MongoDB URL with credentials inside the file. ",
              "Please, DO NOT expose your database credentials to the public ",
              "(for example you can use .gitignore to hide the file).")

      return(FALSE)

    } else {

      assign("log_db",
             mongolite::mongo("shiny_logs", url = readLines(db_url_file)[1]),
             envir = parent.frame()
             )

      options('shinyEventLogger.database' = database)
      message("Logging to the database:       ENABLED: ",
              database, ":", names(
                get('log_db',
                    envir = parent.frame()
                    )$run(command = '{"ping": 1}')
                ))

    } # end of if

  } # end of if

  cat("\n")

  return(TRUE)

} # end of set_logging()

