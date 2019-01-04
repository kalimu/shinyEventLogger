#' Initialize logging to JavaScript console
#'
#' \code{log_init} should be put into the \code{shiny} \code{ui}
#' to initialize JavaScript code that enables logging to JavaScript console
#' in an Internet browser.
#'
#' @return A \code{tagList} with \code{script} tag inside \code{head} tag.
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
#'       set_logging(js_console = TRUE)
#'       log_event("See browser JavaScript console (CTRL + SHIFT + I)")
#'     }
#'   )
#' }
#' }

log_init <- function() {

  jscode_log_event <- '
    Shiny.addCustomMessageHandler("log_event",
      function(message) {
        console.log(message);
      }
    );
  '

  # jscode_log_df <- '
  # Shiny.addCustomMessageHandler("log_df", function(message) {
  #   console.group("data.frame");
  #     console.table(JSON.parse(message));
  #   console.groupEnd();
  # });
  # '

  shiny::tagList(
    shiny::tags$head(shiny::tags$script(
      shiny::HTML(jscode_log_event)
      # shiny::HTML(jscode_log_df)
      )
    )
  )

} # end of log_init
