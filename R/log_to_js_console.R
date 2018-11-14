


log_to_js_console <- function(...) {

  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session))  return()

  args <- list(...)

  event_to_log <- paste0(args, collapse = " ")

  # if (class(args[[1]]) == "json") {

    # session$sendCustomMessage("log_df", as.character(args[[1]]))

  # } else {

  # session$sendCustomMessage("log_this", event_to_log)
  session$sendCustomMessage("log_this", event_to_log)

  # }

} # end of log_to_js_console()

