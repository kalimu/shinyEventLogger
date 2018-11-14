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
