#' Logging unit test
#'
#' \code{log_test} logs a unit test
#' which can be built in inside a shiny app.
#' The event logged has status \code{SUCCESS}
#' or \code{ERROR} if the unit test does not pass successfully.
#' The error status is logged silently and does not stops
#' the shiny app from running by itself.
#' The error message is logged in a log entry body.
#' Deparsed unit test function call is logged
#' as an event name in a log entry header.
#'
#'
#' @inheritParams log_event
#' @param type A character string. A type of the event.
#'   Default for \code{log_test} is \code{"TEST"}.
#' @param ... An unit test function call that is evaluated and logged.
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
#'       set_logging()
#'       log_test(testthat::expect_true(TRUE))
#'       log_test(testthat::expect_true(FALSE))
#'     }
#'   )
#' }
#' }

log_test <- function(...,
                     type   = "TEST",
                     params = NULL
                     ) {

  session <- shiny::getDefaultReactiveDomain()
  input <- session$input

  test_result <-
    tryCatch(expr = {

      eval(..., envir = parent.frame())

      list(
        status = "SUCCESS",
        body   = ""
      )

    }, error = function(e) {

      list(
        status = "ERROR",
        body   = paste0(e, collapse = " ")
      )

    }) # end of tryCatch

  test_result$body <-
    gsub(x = test_result$body,
         pattern = "\n",
         replacement = "")

  event_name <- deparse(substitute(...))

  log_event(
    test_result$body,
    name = event_name,
    type = type,
    status = test_result$status,
    params = params)

} # end of log_test
