
create_event_id <- function(...,
                            name
                            ) {

  if (is.null(name)) {

    paste0(unlist(list(...)), collapse = " ")

  } else {

    name

  } # end if

} # end of create_event_id


#' @export

log_started <- function(...,
                        name = NULL,
                        type = "EVENT",
                        status = "STARTED",
                        params = NULL
                        ) {

  event_counter <- getOption("shinyEventLogger.counter")

  event_id <- create_event_id(..., name = name)

  if (is.null(event_id) || event_id == "") {

    stop("Event must have some kind of unique 'name'",
         "or body passed to the '...' argument.")

  }

  log_event_register[[event_id]] <<-

    list(
      status = status,
      type = type,
      counter = event_counter
    )

  log_event(...,
            name = name,
            type = type,
            status = status,
            params = params,
            event_counter = event_counter)

} # end of log_started


#' @export

log_done <- function(...,
                     name = NULL,
                     params = NULL
                     ) {

  event_counter <- getOption("shinyEventLogger.counter")
  event_id <- create_event_id(..., name = name)

  register <- log_event_register[[event_id]]

  log_event(...,
            name = name,
            type = register$type,
            status = "DONE",
            params = params,
            event_counter = register$counter)

} # end of log_done