

get_event_counter <- function() {

  log_event_register <-
    dynGet("log_event_register",
           minframe = 6L,
           inherits = TRUE,
           ifnotfound = stop(paste0(
             "'log_event_register' not found. ",
             "Have you call 'set_logging_session'?"
           )))

  log_event_register$event_counter


} # end of get_event_counter

increment_event_counter <- function() {

  log_event_register <-
    dynGet("log_event_register",
           minframe = 6L,
           inherits = TRUE)

  log_event_register$event_counter <-
    log_event_register$event_counter + 1


}
