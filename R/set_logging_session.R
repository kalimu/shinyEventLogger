#' Session-specific settings for event logging
#'
#' \code{set_logging_session} should be used at the beginning
#' of the shiny server function to define session-specific event parameters.
#' \code{set_logging_session} also sets event counter to 1.
#'
#' \code{set_logging_session} assigns to the parent frame new environment:
#' \code{log_settings_session} for storing session-specific event parameters
#' and information about multiple
#' instances of the same event (see \code{\link{log_started}}).
#'
#' @family setting up logging parameters functions
#'
#' @inheritParams set_logging
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'
#'   set_logging()
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(log_init()),
#'     server = function(input, output) {
#'       set_logging_session(
#'         session_id = shiny::getDefaultReactiveDomain()$token
#'         )
#'       log_event("Event 1 with session parameter")
#'       log_event("Event 2 with session parameter")
#'     }
#'   )
#'   # You can open app in the browser and duplicate tab to see different
#'   # unique session ids in event parameters.
#' }

set_logging_session <- function(...) {

  params <- eval(list(...))

  assign("log_settings_session",
         list2env(list(
           params = params,
           register = list(),
           event_counter = 1
           )),
         pos = parent.frame())


 TRUE

} # end of set_logging_session
