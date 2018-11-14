


log_to_js_console <- function(...,
                              event_name = NULL,
                              event_type = "EVENT",
                              event_counter = -1
                              ) {

  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) return()

  args <- list(...)

  event_to_log <- paste0(args, collapse = " ")
  event_counter <- paste0("|#", event_counter, "|")
  event_meta <- paste0(event_counter, event_type, "|")

  if (!is.null(event_name)) {

    event_to_log <- gsub(x = event_to_log,
                         pattern = "\n",
                         replacement = paste0("\n", event_counter))

    if (event_to_log != "") {

      event_to_log <-
        paste0(event_counter, event_to_log, "\n", collapse = "\n")

    }

    session$sendCustomMessage(
      type = "log_this",
      message = paste0(event_meta, event_name, "\n", event_to_log,
                       collapse = "\n")
      )

  } else {

    session$sendCustomMessage(
      type = "log_this",
      message = paste0(event_meta, event_to_log, "\n", collapse = " ")
      )

  }

} # end of log_to_js_console()




