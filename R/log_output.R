#' Logging Output of a Function
#'
#'
#' @export
#'
log_output <- function(..., type = "OUTPUT") {

  session <- shiny::getDefaultReactiveDomain()

  input <- session$input

  args <- tryCatch(expr = capture.output(eval(...)),
                   error = function(e) {

                     gsub(x = e, pattern = "\n",replacement = "")

                   })

  event_name <- deparse(substitute(...))

  event_to_log <- paste0(args, collapse = "\n")

  log_event(
    event_to_log,
    name = event_name,
    type = type
    )

} # end of log_output()
