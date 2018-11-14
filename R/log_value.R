#' export
log_value <- function(...) {

  session <- shiny::getDefaultReactiveDomain()

  input <- session$input

  args <- eval(...)

  event_name <- deparse(substitute(...))

  event_to_log <- paste0(args, collapse = "\n")

  log_this(event_to_log,
           event_name = event_name,
           event_type = "VALUE")

}
