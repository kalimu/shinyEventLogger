

log_to_r_console <- function(...,
                             event_name = NULL,
                             event_type = "EVENT",
                             event_counter = -1,
                             status = "DONE",
                             params = NULL
                             ) {

  args <- list(...)

  event_to_log <- paste0(args, collapse = " ")

  event_params <- ""

  if (is.list(params)) {

    event_params <- deparse(params)

  } else if (!is.null(params)) {

    stop("The 'params' argument should be a list, not a ", class(params))

  }

  if (is.null(event_name)) {

    event_name <- event_to_log
    event_to_log <- ""

  }

  log_entry <- create_log_entry(
    event_counter = event_counter,
    event_type    = event_type,
    event_name    = event_name,
    event_status  = status,
    event_params  = event_params,
    event_to_log  = event_to_log
  )

  message(log_entry$header)

  if (!is.null(log_entry$body)) {

    cat(file = stdout(),
        log_entry$body)

  }

  return(
    paste0(log_entry$header,
           ifelse(log_entry$body != "", paste0("\n", log_entry$body), "")
    )
  )

} # end of log_to_r_console()