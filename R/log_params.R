#' @export

log_params <- function(...) {

  params <- eval(list(...))

  assign("log_settings", params, pos = parent.frame())

} # end of log_params
