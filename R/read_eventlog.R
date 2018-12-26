#' @export

read_eventlog <- function(file    = NULL,
                          db      = NULL,
                          last_n  = Inf,
                          verbose = TRUE
                          ) {

  if (all(is.null(file), is.null(db)) ||
      all(!is.null(file), !is.null(db))
      ) {

    stop("You need to define one and only one eventlog source",
         " (`file` or `db`).")

  } # end of if

  if (!is.null(file)) {

    eventlog <-
      read_eventlog_file(file = file,
                         last_n = last_n,
                         verbose = verbose)
  } # end of if

  if (!is.null(db)) {

    eventlog <-
      read_eventlog_db(db = db,
                       last_n = last_n,
                       verbose = verbose)
  } # end of if


  eventlog <-
    bupaR::eventlog(
      eventlog = eventlog,
      case_id = 'session_id',
      activity_id = 'event_name',
      activity_instance_id = 'event_id',
      lifecycle_id = 'event_status',
      timestamp = 'event_timestamp',
      resource_id = 'resource',
      order = "auto"
      )

  eventlog$event_name <-
    gsub(eventlog$event_name, pattern = '"', replacement = "`")
  eventlog$event_name <-
    gsub(eventlog$event_name, pattern = "'", replacement = "`")

  if (last_n != Inf) {

    if (verbose) message("The last event is at the top.")

    eventlog <-
      eventlog[order(eventlog$`.order`,decreasing = TRUE), ]

  }

  eventlog

} # end of read_log

