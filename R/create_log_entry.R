

create_log_entry <- function(event_counter,
                             event_type,
                             event_name,
                             event_status,
                             event_params,
                             event_to_log,
                             separator = "|"
                             ) {

  event_counter <- paste0("#", event_counter)

  event_to_log <- gsub(x = event_to_log,
                       pattern = "\n",
                       replacement =
                         paste0("\n", separator, event_counter, separator)
                       )

  log_entry_header <- paste0(
    separator, event_counter,
    separator, event_type,
    separator, event_name,
    separator, event_status,
    separator, event_params
    )

  if (event_to_log == "") {

    log_entry_body <- ""

  } else {

    log_entry_body <- paste0(
      separator, event_counter,
      separator, event_to_log,
      "\n")

  } # end if

  list(
    header = log_entry_header,
    body   = log_entry_body
  )

} # end of create_log_entry