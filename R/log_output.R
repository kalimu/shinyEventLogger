#' Logging Output of a Function
#'
#'
#' @export

log_output <- function(...,
                       type   = "OUTPUT",
                       status = "FIRED",
                       params = NULL
                       ) {

  session <- shiny::getDefaultReactiveDomain()
  input <- session$input

  args <- tryCatch(

    expr = capture.output(eval(...)),
    # expr = capture.output(invisible(eval(...))),
    # expr = testthat::capture_output(eval(...)),

    error = function(e) {

      gsub(x = e, pattern = "\n",replacement = "")

    }) # end of tryCatch

  event_body <- paste0(args, collapse = "\n")

  event_name <- deparse(substitute(...))

  log_event(
    event_body,
    name = event_name,
    type = type,
    status = status,
    params = params
    )

} # end of log_output()
