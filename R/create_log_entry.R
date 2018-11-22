

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
    separator, event_params

    )

  if (!is.null(event_body) && is.character(event_body) && event_body != "") {


  event_body <-

    gsub(x = event_body,
         pattern = "\n",
         replacement = paste0("\n", separator, event_counter, separator)
         )


    log_entry_body <- paste0(
      separator, event_counter,
      separator, event_body,
      "\n")

  } else {

    log_entry_body <- ""

  } # end if

  list(
    header = log_entry_header,
    body   = log_entry_body
  )

} # end of create_log_entry