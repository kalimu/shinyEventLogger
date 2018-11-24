#' @export

log_value <- function(...,
                      type   = "VALUE",
                      status = "FIRED",
                      params = NULL
                      ) {

  session <- shiny::getDefaultReactiveDomain()
  input <- session$input

  args <- eval(...)
  event_body <- paste0(args, collapse = "\n")

  event_name <- deparse(substitute(...))

  log_event(
    event_body,
    name = event_name,
    type = type,
    status = status,
    params = params
    )

} # end of log_value
