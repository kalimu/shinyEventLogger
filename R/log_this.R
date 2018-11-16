



#' @export

log_this <- function(...,
                     event_name = NULL,
                     event_type = "EVENT",
                     status = "DONE"
                     ) {

  r_console  <- getOption("shinyEventLogger_r_console")
  js_console <- getOption("shinyEventLogger_js_console")
  file       <- getOption("shinyEventLogger_file")
  counter    <- getOption("shinyEventLogger_counter")

  if (!r_console & !js_console & (is.logical(file) && !file)) return()

  if (r_console) {

    log_to_r_console(...,
                     event_name = event_name,
                     event_type = event_type,
                     event_counter = counter,
                     status = status
                     )

  } # end of if r_console

  if (js_console) {

    log_to_js_console(...,
                      event_name = event_name,
                      event_type = event_type,
                      event_counter = counter,
                      status = status
                      )
  } # end of if js_console

  if ((is.logical(file) && file) || is.character(file)) {

    log_to_file(...,
                event_name = event_name,
                event_type = event_type,
                event_counter = counter,
                status = status
                )

  } # end of if file

  if (status == "DONE") options('shinyEventLogger_counter' = counter + 1)

  list('event_counter' = counter)

} # end of log_this()





