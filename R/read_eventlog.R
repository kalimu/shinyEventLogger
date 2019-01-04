#' Reading eventlog
#'
#' \code{read_eventlog} reads eventlog stored in a file or in a database.
#'
#' @return An object of class \code{eventlog} which is a data frame with
#' appropriate case, activity and timestamp classifiers specified.
#' The \code{eventlog} object is a result of
#' \code{\link[bupaR:eventlog]{bupaR:eventlog}} function from \code{bupaR}
#' package and it is suitable for further process-mining analysis.
#'
#' @param file A character string. Path to a file log.
#' @param db A character string. Connection string to a mongo database.
#' @param last_n An integer. How many last event records should be return?
#'   Default is \code{Inf} which returns the whole eventlog.
#' @param verbose A logical value. Should the function print addition messages?
#'   Default is TRUE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' read_eventlog(
#'  last_n = 25,
#'  file = system.file("shiny", "demoapp/events.log",
#'                      package = "shinyEventLogger"))
#' }

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

