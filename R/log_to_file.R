#' @family low-level logging functions

log_to_file <- function(header,
                        body = "",
                        event_timestamp,
                        session_id = ""
                        ) {

  if (missing(header)) stop("A header of log entry is missing.")

  file <- getOption("shinyEventLogger.file")

  if (is.null(file) || !is.character(file)) {

    warning("Use set_logging() to define log file.")
    return(FALSE)

  }

  if (!file.exists(file)) {

    warning("File log '", file ,"' does NOT exist!")
    return(FALSE)

  } # end of if

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

