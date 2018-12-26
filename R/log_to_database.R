
log_to_database <- function(event_counter,
                            event_type,
                            event_name,
                            event_status,
                            event_params,
                            event_body,
                            event_timestamp,
                            session_id
                            ) {

  database <- getOption("shinyEventLogger.database")

  if (is.null(database) || !is.character(database)) {

    warning("Use set_logging() to define log database.")
    return(FALSE)

  }

  log_db <- dynGet('log_db', minframe = 6L, inherits = TRUE)

  event_body <- ifelse(is.null(event_body), NA, event_body)

  json <-
    jsonlite::toJSON(POSIXt = "mongo", list(
      event_counter   = jsonlite::unbox(event_counter),
      event_type      = jsonlite::unbox(event_type),
      event_name      = jsonlite::unbox(event_name),
      event_status    = jsonlite::unbox(event_status),
      event_params    = event_params,
      event_body      = jsonlite::unbox(event_body),
      event_timestamp = jsonlite::unbox(as.POSIXct(event_timestamp)),
      session_id      = jsonlite::unbox(session_id)
      ))

  log_db$insert(json)

} # end of log_to_database

