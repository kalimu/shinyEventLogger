

get_event_counter <- function() {

 event_register <-
    dynGet("log_event_register",
           minframe = 1L,
           inherits = TRUE,
           ifnotfound = stop(paste0(
             "'log_event_register' not found. ",
             "Have you call 'set_logging_session'?"
           )))

   event_register <-
  as.list(event_register)

  event_register$event_counter

} # end of get_event_counter

increment_event_counter <- function(event_counter) {

  event_register <-
    dynGet("log_event_register",
           # minframe = 6L,
           minframe = 1L,
           inherits = TRUE)

  event_counter_registered <-
    as.list(event_register)$event_counter

  # if (event_counter == event_counter_registered) {

  event_register$event_counter <- event_counter_registered + 1

  # }


} # end of increment_event_counter
