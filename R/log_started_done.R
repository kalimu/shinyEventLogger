
create_event_id <- function(...,
                            name
                            ) {

  if (is.null(name)) {

    paste0(unlist(list(...)), collapse = " ")

  } else {

    name

  } # end if

} # end of create_event_id



#' Logging the start of an event
#'
#' \code{log_started} logs an event with status \code{"STARTED"}.
#' \code{log_done} logs the same event with status \code{"DONE"}.
#' Difference between timestamps of these two log entries can be used
#' for timing an event. One event can have several instances with
#' different statuses.
#' When logging instances of the same event, event \code{name}
#' or, if \code{name = NULL}, objects passed to \code{...}
#' must be exactly the same, as they are used to create unique event id.
#' Started events, their types and counters are registered
#' in an environment called \code{log_event_register},
#' which enables creating and timing multiple nested events.
#'
#'
#'
#' @inheritParams log_event
#' @param status A character string. A status of the event.
#'   Default for \code{log_started} is \code{"STARTED"}.
#'   The \code{status} is always \code{"DONE"} when using \code{log_done}.
#' @param type A character string. A type of the event.
#'   Default for \code{log_started} is \code{"EVENT"}.
#'   The \code{type} logged with \code{log_done} is the same as
#'   the \code{type} of the event logged with \code{log_started}.
#'
#' @describeIn log_started Logging the start of an event
#'
#' @family logging events functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(log_init()),
#'     server = function(input, output) {
#'       options(digits.secs = 6)
#'       set_logging()
#'         log_started(as.character(Sys.time()), name = "Event 1")
#'         log_started(as.character(Sys.time()), name = "Event 2")
#'           log_event(as.character(Sys.time()), name = "Event 3")
#'            log_done(as.character(Sys.time()), name = "Event 2")
#'            log_done(as.character(Sys.time()), name = "Event 1")
#'     }
#'   )
#' }
#' }

log_started <- function(...,
                        name = NULL,
                        type = "EVENT",
                        status = "STARTED",
                        params = NULL
                        ) {

  event_counter <- getOption("shinyEventLogger.counter")

  event_id <- create_event_id(..., name = name)

  if (is.null(event_id) || event_id == "") {

    stop("Event must have some kind of unique 'name'",
         "or body passed to the '...' argument.")

  }

  log_event_register <- get('log_event_register', envir = parent.frame())

  log_event_register[[event_id]] <-

    list(
      status = status,
      type = type,
      counter = event_counter
    )

  log_event(...,
            name = name,
            type = type,
            status = status,
            params = params,
            event_counter = event_counter)

} # end of log_started

#' @describeIn log_started Logging the end of an event
#' @export

log_done <- function(...,
                     name = NULL,
                     params = NULL
                     ) {

  event_id <- create_event_id(..., name = name)

  log_event_register <- get('log_event_register', envir = parent.frame())

  register <- log_event_register[[event_id]]

  log_event(...,
            name = name,
            type = register$type,
            status = "DONE",
            params = params,
            event_counter = register$counter)

} # end of log_done