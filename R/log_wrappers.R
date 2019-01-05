#' Logging a message
#'
#' \code{log_message}, \code{log_warning}, and \code{log_error} are
#' wrapper functions for logging events of type
#' \code{MESSAGE}, \code{WARNING}, or \code{ERROR}.
#' Relevant message, warning or error is raised after logging an event.
#' Raising an error is done using \code{\link{stop}} function
#' and it can stop the whole shiny app.
#'
#' @inheritParams log_event
#' @param ... Objects that are evaluated, coerced into character string,
#' collapsed and pasted as event name into log entry header.
#' The character string is also passed to the
#' message, warning, or error raised.
#'
#' @describeIn log_message Logging a message
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
#'       log_message("Example of a message")
#'       log_warning("Example of a warning")
#'       log_error("Example of an error")
#'     }
#'   )
#' }
#' }

log_message <- function(...) {

  log_event(..., type = "MESSAGE")

  message(...)

}

#' @describeIn log_message Logging a warning
#' @export

log_warning <- function(...) {

  log_event(..., type = "WARNING")

  warning(...)

}

#' @describeIn log_message Logging an error
#' @export

log_error <- function(...) {

  log_event(..., type = "ERROR")

  stop(...)

}
