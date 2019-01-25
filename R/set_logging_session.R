#' Session-specific settings for event logging
#'
#' \code{set_logging_session} should be used at the beginning
#' of the shiny server function to define session-specific event parameters.
#' \code{set_logging_session} also sets event counter to 1.
#'
#' \code{set_logging_session} assigns to the parent frame new environments:
#' \code{log_settings_session} for storing session-specific event parameters
#' and \code{log_event_register} for storing information about multiple
#' instances of the same event (see \code{\link{log_started}}).
#'
#' @family setting up logging parameters functions
#'
#' @inheritParams set_logging
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'
#'   set_logging()
#'
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(log_init()),
#'     server = function(input, output) {
#'       set_logging_session(
#'         session_id = shiny::getDefaultReactiveDomain()$token
#'         )
#'       log_event("Event with global params")
#'     }
#'   )
#'   # You can open app in the browser and duplicate tab to see different
#'   # unique session ids in event parameters.
#' }
#' }

set_logging_session <- function(...) {

  params <- eval(list(...))
  assign("log_settings_session", list2env(params), pos = parent.frame())

  assign("log_event_register",
         # new.env(parent = emptyenv()),
         list2env(list(event_counter = 1)),
         envir = parent.frame())

 TRUE

} # end of set_logging_session
