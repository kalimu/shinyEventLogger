



#' @export

log_event <- function(...,
                     name = NULL,
                     type = "EVENT",
                     status = "FIRED",
                     params = NULL
                     ) {

  r_console        <- getOption("shinyEventLogger.r_console")
  js_console       <- getOption("shinyEventLogger.js_console")
  file             <- getOption("shinyEventLogger.file")
  event_counter    <- getOption("shinyEventLogger.counter")

  if (!r_console & !js_console & (is.logical(file) && !file)) return()

  if (!status %in% c("STARTED")) {

    options('shinyEventLogger.counter' = event_counter + 1)

  }


  args <- list(...)

  event_body <- paste0(args, collapse = " ")


  if (!is.null(params) && !is.list(params)) {

    stop("The 'params' argument should be a list, not a ", class(params))

  } else if (is.list(params)) {

    event_params <- deparse(params)

  } else {

    event_params <- NULL

  } # end if


  if (is.null(name)) {

    event_name <- event_body
    event_body <- NULL

  } else {

    event_name <- name

  } # end if

  event_status <- status
  event_type   <- type

  log_entry <- create_log_entry(

    event_counter = event_counter,
    event_type    = event_type,
    event_name    = event_name,
    event_status  = event_status,
    event_params  = event_params,
    event_body    = event_body

  ) # end of create_log_entry




  # TODO: expect_error(fixed = TRUE,
  #   shinyEventLogger:::log_to_r_console(params = "a = 1"),
  #   "The 'params' argument should be a list, not a character"
  #   )

  #
  # expect_message(fixed = TRUE,
  #   shinyEventLogger:::log_to_r_console("Test", "compound", "event"),
  #   "|#-1|EVENT|Test compound event|DONE|"
  #   )
  if (r_console) {

    log_to_r_console(header = log_entry$header, body = log_entry$body)

  } # end of if r_console

  if (js_console) {

    log_to_js_console(header = log_entry$header, body = log_entry$body)

  } # end of if js_console

  if ((is.logical(file) && file) || is.character(file)) {

    log_to_file(header = log_entry$header, body = log_entry$body)

  } # end of if file



  list('event_counter' = event_counter)

} # end of log_event()





