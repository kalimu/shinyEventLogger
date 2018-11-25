#' @export

log_params <- function(...) {

    log_settings <- new.env(parent = parent.frame(2))

    params <- as.list(match.call())

    log_settings <<- list2env(x = params[-1], parent = parent.frame(2))

} # end of log_params
