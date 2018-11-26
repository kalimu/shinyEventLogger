#' @export

log_event <- function(...,
                     name = NULL,
                     type = "EVENT",
                     status = "FIRED",
                     params = NULL,
                     event_counter = getOption("shinyEventLogger.counter")
                     ) {

  r_console        <- getOption("shinyEventLogger.r_console")
  js_console       <- getOption("shinyEventLogger.js_console")
  file             <- getOption("shinyEventLogger.file")

  if (any(c(is.null(r_console),
            is.null(js_console),
            is.null(file),
            is.null(event_counter)
            ))) {

   stop("Use set_logging() before logging events.")

  }

  to_return <-
    list(
      counter = NULL,
      entry   = NULL
    )

  if (!r_console &
      !js_console &
      (is.logical(file) && !file)
      ) {

    return(to_return)

  } # end if

  # event_body ###############################################################
  args <- list(...)
  event_body <- paste0(args, collapse = " ")

  # event_params #############################################################
  if (!is.null(params) && !is.list(params)) {

    stop("The 'params' argument should be a list, not a ", class(params))

  } else if (is.list(params)) {

    event_params <- params

  } else {

    event_params <- NULL

  } # end if

  if (exists('log_settings', envir = parent.frame(1))) {

    event_params <- c(params, get('log_settings', envir = parent.frame(1)))

  } else if (exists('log_settings', envir = parent.frame(2))) {

    event_params <- c(params, get('log_settings', envir = parent.frame(2)))

  } else if (exists('log_settings', envir = parent.frame(3))) {

    event_params <- c(params, get('log_settings', envir = parent.frame(3)))

  } # end if

  if (NROW(event_params) > 0) {

    event_params <- deparse(event_params)

  } else {

    event_params <- NULL

  }

  # event_name ################################################################
  if (is.null(name)) {

    event_name <- event_body
    event_body <- NULL

  } else {

    event_name <- name

  } # end if

  # event_status ##############################################################
  event_status <- status

  # event_type ################################################################
  event_type   <- type

  # log_entry #################################################################
  log_entry <- create_log_entry(

    event_counter = event_counter,
    event_type    = event_type,
    event_name    = event_name,
    event_status  = event_status,
    event_params  = event_params,
    event_body    = event_body

  ) # end of create_log_entry

  # log_to_[...] ##############################################################
  if (r_console) {

    result_r_console <-
      log_to_r_console(header = log_entry$header,
                       body = log_entry$body)

  } # end if

  if (js_console) {

    result_js_console <-
      log_to_js_console(header = log_entry$header,
                        body = log_entry$body)

  } # end if

  if ((is.logical(file) && file) || is.character(file)) {

    result_file <-
      log_to_file(header = log_entry$header,
                  body = log_entry$body)

  } # end if

  to_return$counter <- event_counter
  to_return$entry   <- result_r_console

  if (event_counter == getOption("shinyEventLogger.counter")) {

    options(
      'shinyEventLogger.counter' =
        getOption("shinyEventLogger.counter") + 1
      )

  } # end of if

  return(to_return)

} # end of log_event()



