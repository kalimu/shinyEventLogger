
#' @export

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
