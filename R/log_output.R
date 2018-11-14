
#' export

log_output <- function(..., event_type = "OUTPUT") {

  session <- shiny::getDefaultReactiveDomain()

  input <- session$input

  args <- tryCatch(expr = capture.output(...),
                   error = function(e) {

                     gsub(x = e, pattern = "\n",replacement = "")

                   })

  event_name <- deparse(substitute(...))

  event_to_log <- paste0(args, collapse = "\n")

  log_this(event_to_log,
           event_name = event_name,
           event_type = event_type)

} # end of log_output()
