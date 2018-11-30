

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
