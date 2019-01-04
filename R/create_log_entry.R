#' Create log entry
#'
#' \code{create_log_entry} creates a standardized log entry.
#'
#' Function \code{create_log_entry} takes as arguments
#' different information about a single event and
#' creates standardized log entry header and body
#' used to be displayed in R console, browser JavaScript console,
#' or as a one-line log record in the log file
#' (after additional conversion from multi-line output).
#'

create_log_entry <- function(event_name = NULL,
                             event_body = NULL,
                             event_type = "EVENT",
                             event_status = "DONE",
                             event_params = NULL,
                             event_counter = -1,
                             separator = "|"
                             ) {

  event_counter <- paste0("#", event_counter)

  log_entry_header <- paste0(

    separator, event_counter,
    separator, event_type,
    separator, event_name,
    separator, event_status,
    separator

    )

  log_entry_body <- ""

  if (!is.null(event_params)) {

    event_params <- deparse(event_params, width.cutoff = 500)

    log_entry_body <- paste0(
      separator, event_counter,
      separator, "PARAMS",
      separator, event_params,
      "\n"
      )

  }

  if (!is.null(event_body) && is.character(event_body) && event_body != "") {

    event_body <- gsub(
      x = event_body,
      pattern = "\n",
      replacement = paste0("\n", separator, event_counter, separator)
      )

    log_entry_body <- paste0(
      log_entry_body,
      separator, event_counter,
      separator, event_body, "\n"
      )

  } # end if

  list(
    header = log_entry_header,
    body   = log_entry_body
  )

} # end of create_log_entry