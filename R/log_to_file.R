
log_to_file <- function(...,
                        event_name = NULL,
                        event_type = "EVENT",
                        event_counter = -1,
                        status = "DONE",
                        params = NULL
                        ) {

  file <- getOption("shinyEventLogger_file")

  if (!file.exists(file)) {

    if (file.create(file)) {

      message("New log file: ", file, " has been created.")

    } else {

      warning("Unable to create log file.")

    }

  }

  session_id <- NA
  session <- shiny::getDefaultReactiveDomain()

  if (!is.null(session)) {

    session_id <- session$token

  }

  # session_id <- paste0("|", session_id)
  event_timestamp <- paste0(format(as.numeric(Sys.time()), nsmall = 10))

  args <- list(...)
  event_to_log <- paste0(args, collapse = " ")

  event_counter <- paste0("#", event_counter, "|")

  event_meta <- paste0(event_counter, event_type, "|")

  event_params <- ""

  if (is.list(params)) {

    event_params <- deparse(params)

  }

  if (!is.null(event_name)) {

    event_to_log <- gsub(x = event_to_log,
                         pattern = "\n",
                         replacement = " ")

    if (event_to_log == "") {

      event_to_log <- "NA"

    }

    event_to_log <-
      paste0(event_meta,
             event_name, "|",
             status, "|",
             event_params, "|",
             session_id, "|",
             event_timestamp, "|",
             event_to_log,
             "|", "\n",
             collapse = " ")

  } else {

    event_to_log <-
      paste0(event_meta,
             event_to_log, "|",
             status, "|",
             event_params, "|",
             session_id, "|",
             event_timestamp, "|",
             "|", "\n",
             collapse = " ")

  }

  cat(event_to_log, file = file, append = TRUE)

} # end of log_to_file

