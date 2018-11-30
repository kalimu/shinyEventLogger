#' @export

log_test <- function(...,
                     type   = "TEST",
                     status = NULL,
                     params = NULL
                     ) {

  session <- shiny::getDefaultReactiveDomain()
  input <- session$input

  test_result <-
    tryCatch(expr = {

      eval(...)

      list(
        status = "SUCCESS",
        body   = ""
      )

    }, error = function(e) {

      list(
        status = "ERROR",
        body   = paste0(e, collapse = " ")
      )

    }) # end of tryCatch

  test_result$body <-
    gsub(x = test_result$body,
         pattern = "\n",
         replacement = "")

  event_name <- deparse(substitute(...))

  log_event(
    test_result$body,
    name = event_name,
    type = type,
    status = test_result$status,
    params = params)

} # end of log_test
