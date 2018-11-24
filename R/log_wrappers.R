#' @export
log_started <- function(...) {

  log_event(..., status = "STARTED")

}

#' @export
log_done <- function(...) {

  log_event(..., status = "DONE")

}

#' @export
log_test <- function(...) {

  log_output(..., type = "TEST")

}


#' @export
log_message <- function(...) {

  log_event(..., type = "MESSAGE")

  message(...)

}

#' @export
log_warning <- function(...) {

  log_event(..., type = "WARNING")

  warning(...)

}


#' @export
log_error <- function(...) {

  log_event(..., type = "ERROR")

  stop(...)

}
