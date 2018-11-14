
#' @export



log_this <- function(..., event_type = "EVENT") {

  r_console  <- getOption("shinyEventLogger_r_console")
  js_console <- getOption("shinyEventLogger_js_console")
  file       <- getOption("shinyEventLogger_file")

  if (!r_console & !js_console & (is.logical(file) && !file)) return()

  if (r_console) {
    log_to_r_console(..., event_type = event_type)
  }

  if (js_console) {
    log_to_js_console(..., event_type = event_type)
  }

  if ((is.logical(file) && file) || is.character(file)) {
    log_to_file(..., event_type = event_type)
  }

} # end of log_this()





