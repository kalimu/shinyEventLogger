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
#' \code{log_setting} which is assigned to the environment from which the
#' \code{log_params} function was called for.
#'
#' @param ... a set of named objects
#'   (usually of type character, numeric, or date)
#'
#' @export

#' @examples
#' \dontrun{
#'
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(log_init()),
#'     server = function(input, output) {
#'       set_logging()
#'       observe({
#'         log_params("observer" = 1)
#'         log_event("Event 1")
#'       })
#'       observe({
#'         log_params("observer" = 2)
#'         log_event("Event 1")
#'       })
#'     }
#'   )
#' }
#' }

log_params <- function(...) {

  params <- eval(list(...))

  assign("log_settings", list2env(params), pos = parent.frame())

} # end of log_params
