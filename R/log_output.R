#' Logging output of a function
#'
#' \code{log_output} logs output of a function into log entry body
#' and uses deparsed function call as the event name.
#'
#' @inheritParams log_event
#' @param type A character string. A type of the event.
#'   Default is \code{"OUTPUT"}.
#' @param ... A function call that is evaluated, coerced into character string,
#'   collapsed and pasted as multi-line text into log entry body.
#'   Deparsed function call is used as the event name in log entry header.
#'
#' @family logging events functions
#'
#' @importFrom utils capture.output
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' if (interactive()) {
#'   set_logging()
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(log_init()),
#'     server = function(input, output) {
#'       set_logging_session()
#'       log_output(NROW(mtcars))
#'       log_output(head(mtcars))
#'     }
#'   )
#'
#' }
#' }

log_output <- function(...,
                       type   = "OUTPUT",
                       status = "FIRED",
                       params = NULL
                       ) {

  session <- shiny::getDefaultReactiveDomain()
  input <- session$input

  args <- tryCatch(

    expr = capture.output(force(...)),

    error = function(e) {

      gsub(x = e, pattern = "\n",replacement = "")

    }) # end of tryCatch

  event_body <- paste0(args, collapse = "\n")

  event_name <- deparse(substitute(...))

  log_event(
    event_body,
    name = event_name,
    type = type,
    status = status,
    params = params
    )

} # end of log_output()
