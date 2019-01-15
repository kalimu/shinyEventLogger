#' Logging an event
#'
#' \code{log_event} logs an event into R console, browser JavaScript console,
#' file, or database
#' depending on user's settings (see \code{\link{set_logging}}).
#'
#' @param ... Objects that are evaluated, coerced into character string,
#'   collapsed and pasted into log entry body
#'   (or header if \code{name} is \code{NULL}).
#' @param name A character string. The name of the event.
#' @param type A character string. A type of the event.
#'   Default is \code{"EVENT"}.
#' @param status A character string. A status of the event.
#'   Default is \code{"FIRED"}.
#' @param params A list of additional named event-specific parameters.
#'   Default is \code{NULL}.
#' @param event_counter An integer. The number of the event.
#' By default current value of the counter is returned by
#' the internal getter function \code{get_event_counter}.
#'
#' @family logging events functions
#' @seealso
#'   \code{\link{set_logging}} for setting event logging,
#'   \code{\link{log_init}} for initialize JavaScript logging in shiny app,
#'   \code{\link{log_params}} for setting scope-specific event parameters,
#'   \code{\link{read_eventlog}} for reading eventlog from a file or a database.
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
#'       log_event("Event 1")
#'       log_event("Event 2 body", name = "Event 2")
#'       log_event("Event 3", type = "NewTYPE")
#'       log_event("Event 4", status = "EXECUTED")
#'       log_event("Event 5", event_counter = 123)
#'     }
#'   )
#' }
#' }

log_event <- function(...,
                     name = NULL,
                     type = "EVENT",
                     status = "FIRED",
                     params = NULL,
                     event_counter = get_event_counter()
                     ) {

  r_console        <- getOption("shinyEventLogger.r_console")
  js_console       <- getOption("shinyEventLogger.js_console")
  file             <- getOption("shinyEventLogger.file")
  database         <- getOption("shinyEventLogger.database")

  if (any(c(is.null(r_console),
            is.null(js_console),
            is.null(file),
            is.null(database),
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
      (is.logical(database) && !database) &
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

  add_parent_params <- function(envir_name, event_params) {

    if (exists(envir_name, envir = parent.frame(1))) {

      event_params <- c(event_params,
                        as.list(get(envir_name, envir = parent.frame(1))))

    } else if (exists(envir_name, envir = parent.frame(2))) {

      event_params <- c(event_params,
                        as.list(get(envir_name, envir = parent.frame(2))))

    } else if (exists(envir_name, envir = parent.frame(3))) {

      event_params <- c(event_params,
                        as.list(get(envir_name, envir = parent.frame(3))))

    } # end if

      event_params

  } # end of add_parent_params

  event_params <- add_parent_params(envir_name = "log_settings",
                                    event_params)

  event_params <- add_parent_params(envir_name = "log_settings_session",
                                    event_params)

  event_params <- add_parent_params(envir_name = "log_settings_global",
                                    event_params)


  if (NROW(event_params) > 0 || is.environment(event_params)) {

    event_params <- as.list(event_params)

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
                       body   = log_entry$body)

  } # end if

  if (js_console) {

    result_js_console <-
      log_to_js_console(header = log_entry$header,
                        body   = log_entry$body)

  } # end if

  session <- shiny::getDefaultReactiveDomain()

  if (!is.null(session)) {

    session_id <- session$token

  } else {

    session_id <- ""

  } # end of if

  event_timestamp <- Sys.time()
  attr(event_timestamp, "tzone") <- "UTC"

  if ((is.logical(file) && file) || is.character(file)) {

    result_file <-
      log_to_file(header          = log_entry$header,
                  body            = log_entry$body,
                  session_id      = session_id,
                  event_timestamp = event_timestamp
                  )

  } # end if

  if ((is.logical(database) && database) || is.character(database)) {

    result_databse <-
      log_to_database(
        event_counter   = event_counter,
        event_type      = event_type,
        event_name      = event_name,
        event_status    = event_status,
        event_params    = event_params,
        event_body      = event_body,
        event_timestamp = event_timestamp,
        session_id      = session_id

      )


  } # end if

  to_return$counter <- event_counter
  to_return$entry   <- result_r_console

  if (event_counter == get_event_counter()) {

    increment_event_counter()

  } # end of if

  return(to_return)

} # end of log_event()



