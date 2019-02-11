#' Logging value
#'
#' \code{log_value} logs value of an evaluated function into log entry body
#' and uses deparsed function call as the event name.
#'
#' @inheritParams log_event
#' @param type A character string. A type of the event.
#'   Default is \code{"VALUE"}.
#' @param ... A function call that is evaluated, the returned value is
#'   coerced into character string,
#'   and pasted into log entry body.
#'   Deparsed function call is used as the event name in log entry header.
#'
#' @family logging events functions
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   set_logging()
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(log_init()),
#'     server = function(input, output) {
#'       set_logging_session()
#'       log_value(NROW(mtcars))
#'     }
#'   )
#'
#' }

log_value <- function(...,
                      type   = "VALUE",
                      status = "FIRED",
                      params = NULL
                      ) {

  session <- shiny::getDefaultReactiveDomain()
  input <- session$input

  args <- eval(...)
  event_body <- paste0(args, collapse = "\n")

  event_name <- deparse(substitute(...))

  log_event(
    event_body,
    name = event_name,
    type = type,
    status = status,
    params = params
    )

} # end of log_value
