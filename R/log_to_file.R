
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

  params <- ""
  output <- ""

  if (body != "") {

    body <- as.list(strsplit(body, split = "\\|#.{1,}?\\|")[[1]][-1])

    if (any(grepl(x = body, pattern = "PARAMS"))) {

      params <- body[grep(x = body, pattern = "PARAMS")]

      params <- gsub(x = params,
                     pattern = "PARAMS\\|",
                     replacement = "")

      params <- gsub(x = params,
                     pattern = "\n",
                     replacement = "")

    }

    output <-  body[!grepl(x = body, pattern = "PARAMS")]

    output <- gsub(x = output,
                   pattern = "\n",
                   replacement = "")

    output <- paste0(output, collapse = "\t")

  } # end of if

  cat(paste0(header,
             params, "|",
             session_id, "|",
             event_timestamp, "|",
             output, "|",
             "\n"
             ),
      file = file,
      append = TRUE
      )

  return(TRUE)

} # end of log_to_file

