#' Logging scope-specific parameters of events
#'
#' With \code{log_params} you can define a set of named parameters,
#' which are common for events from the same scope
#' (for example inside an observer).
#' These parameters will be added to event-specific parameters
#' and logged within the same log entry.
#'
#' The function takes all objects passed inside \code{...} argument,
#' evaluates them, and stores them in a new environment called
#' \code{log_setting} which is assigned to the parent environment
#' from which the \code{log_params} function was called.
#'
#' @param ... a set of named objects
#'   (usually of type character, numeric, or date)
#'  to be logged as event parameters.
#'
#' @family setting up logging parameters functions
#'
#' @export

#' @examples
#' \dontrun{
#'
#' if (interactive()) {
#'   set_logging()
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(log_init()),
#'     server = function(input, output) {
#'       set_logging_session()
#'       observe({
#'         log_params("observer" = "A")
#'         log_event("Event A.1")
#'         log_event("Event A.2")
#'       })
#'       observe({
#'         log_params("observer" = "B")
#'         log_event("Event B.1")
#'         log_event("Event B.2")
#'       })
#'     }
#'   )
#' }
#' }

log_params <- function(...) {

  params <- eval(list(...))

  assign("log_settings", list2env(params), pos = parent.frame())

} # end of log_params
