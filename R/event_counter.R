

get_event_counter <- function() {

  log_event_register <-
    dynGet("log_event_register",
           minframe = 1L,
           inherits = FALSE,
           ifnotfound = stop(paste0(
             "'log_event_register' not found. ",
             "Have you call 'set_logging_session'?"
           )))

  as.list(log_event_register)$event_counter

} # end of get_event_counter

increment_event_counter <- function() {

  log_event_register <-
    dynGet("log_event_register",
           # minframe = 6L,
           minframe = 1L,
           inherits = FALSE)

  event_counter <-
    as.list(log_event_register)$event_counter

  log_event_register$event_counter <-
    event_counter + 1

} # end of increment_event_counter
