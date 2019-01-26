

get_event_counter <- function() {

  log_event_register <-
    dynGet("log_event_register",
           minframe = 0L,
           inherits = TRUE,
           ifnotfound = stop(paste0(
             "'log_event_register' not found. ",
             "Have you call 'set_logging_session'?"
           )))

  as.list(log_event_register)$event_counter

} # end of get_event_counter

increment_event_counter <- function(current_counter) {

  log_event_register <-
    dynGet("log_event_register",
           # minframe = 6L,
           minframe = 0L,
           inherits = TRUE)

  log_event_register$event_counter <- current_counter + 1

} # end of increment_event_counter
