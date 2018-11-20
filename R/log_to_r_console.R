

log_to_r_console <- function(...,
                             event_name = NULL,
                             event_type = "EVENT",
                             event_counter = -1,
                             status = "DONE",
                             params = NULL
                             ) {



  args <- list(...)

  event_to_log <- paste0(args, collapse = " ")

  event_counter <- paste0("|#", event_counter, "|")

  event_meta <- paste0(event_counter, event_type, "|")

  event_params <- ""

  if (is.list(params)) {

    event_params <- deparse(params)

  }

  if (!is.null(event_name)) {

    cat(file = stderr(),
        paste0(event_meta,
               event_name, "|" ,
               status, "|",
               event_params, "\n",
               collapse = " ")
        )

    event_to_log <- gsub(x = event_to_log,
                         pattern = "\n",
                         replacement = paste0("\n", event_counter))

    if (event_to_log != "") {

      cat(file = stdout(),
          paste0(event_counter, event_to_log, "\n",
                 collapse = paste0("\n"))
          )
    }

  } else {

    cat(file = stderr(),
        paste0(event_meta,
               event_to_log, "|",
               status, "|",
               event_params, "\n",
               collapse = " ")
        )

  }


} # end of log_to_r_console()