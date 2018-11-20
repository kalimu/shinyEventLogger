#' @export
log_started <- function(...) {

  log_this("Rendering table", status = "STARTED")

}

#' @export
log_done <- function(...) {

  log_this("Rendering table", status = "DONE")

}

#' @export
log_test <- function(...) {

  log_output(..., event_type = "TEST")

}


#' @export
log_message <- function(...) {

  log_this(..., event_type = "MESSAGE")
  message(...)

}

#' @export
log_warning <- function(...) {

  log_this(..., event_type = "WARNING")
  warning(...)

}


#' @export
log_error <- function(...) {

  log_this(..., event_type = "ERROR")
  stop(...)

}
