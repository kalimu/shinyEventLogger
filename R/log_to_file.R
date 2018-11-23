
log_to_file <- function(header,
                        body = ""
                        ) {

  if (missing(header)) stop("A header of log entry is missing.")

  file <- getOption("shinyEventLogger.file")

  if (is.null(file) || !is.character(file)) {

    stop("Use set_logging() to define log file.")

  }

  if (!file.exists(file)) {

    stop("File log '", file ,"' does NOT exist!")

  } # end of if

  session <- shiny::getDefaultReactiveDomain()

  if (!is.null(session)) {

    session_id <- session$token

  } else {

    session_id <- ""

  } # end of if

  event_timestamp <- paste0(format(as.numeric(Sys.time()), nsmall = 10))

  if (body != "") {

    body <- gsub(x = body,
                 pattern = "\n",
                 replacement = "")

    body <- paste0(
      deparse(as.list(strsplit(body, split = "\\|#.{1,}?\\|")[[1]][-1])),
      collapse = ""
      )

  } # end of if

  cat(paste0(header,
             session_id, "|",
             event_timestamp, "|",
             body, "\n"
             ),
      file = file,
      append = TRUE
      )

  return(TRUE)

} # end of log_to_file

