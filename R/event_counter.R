

get_event_counter <- function() {

 settings_session <-
    # dynGet("log_event_register",
    dynGet("log_settings_session",
           minframe = 1L,
           inherits = TRUE,
           ifnotfound = stop(paste0(
             "'log_settings_session' not found. ",
             "Have you call 'set_logging_session'?"
           )))

  settings_session$event_counter

} # end of get_event_counter

increment_event_counter <- function() {

  settings_session <-
    dynGet("log_settings_session",
           # minframe = 6L,
           minframe = 1L,
           inherits = TRUE)


  settings_session$event_counter <-
    settings_session$event_counter + 1

} # end of increment_event_counter
