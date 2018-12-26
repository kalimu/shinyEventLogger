
#' @export

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
  options('shinyEventLogger.counter'    = 1)

  global_params <- eval(list(...))

  assign("log_settings_global",
         list2env(global_params),
         envir = parent.frame())

  assign("log_event_register",
         new.env(parent = emptyenv()),
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

    db_url_file = ".db_url"

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
             mongolite::mongo("demo", url = readLines(db_url_file)[1]),
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

