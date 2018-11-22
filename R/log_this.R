



#' @export

log_this <- function(...,
                     event_name = NULL,
                     event_type = "EVENT",
                     status = "FIRED",
                     params = NULL
                     ) {

  r_console  <- getOption("shinyEventLogger.r_console")
  js_console <- getOption("shinyEventLogger.js_console")
  file       <- getOption("shinyEventLogger.file")
  counter    <- getOption("shinyEventLogger.counter")

  if (!r_console & !js_console & (is.logical(file) && !file)) return()

  if (!status %in% c("STARTED")) {

    options('shinyEventLogger.counter' = counter + 1)

  }

  if (!is.null(params) && !is.list(params)) {

    stop("The 'params' argument should be a list, not a ", class(params))

  }

  if (r_console) {

    log_to_r_console(...,
                     event_name = event_name,
                     event_type = event_type,
                     event_counter = counter,
                     status = status,
                     params = params
                     )

  } # end of if r_console

  if (js_console) {

    log_to_js_console(...,
                      event_name = event_name,
                      event_type = event_type,
                      event_counter = counter,
                      status = status,
                      params = params
                      )
  } # end of if js_console

  if ((is.logical(file) && file) || is.character(file)) {

    log_to_file(...,
                event_name = event_name,
                event_type = event_type,
                event_counter = counter,
                status = status,
                params = params
                )

  } # end of if file



  list('event_counter' = counter)

} # end of log_this()





