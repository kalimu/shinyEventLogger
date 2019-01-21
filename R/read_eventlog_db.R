#' @importFrom mongolite mongo

read_eventlog_db <- function(db = readLines(".db_url")[1],
                             last_n = Inf,
                             verbose = TRUE
                             ) {

  log_db <- mongolite::mongo("demo", url = db)

  if (last_n != Inf) {

   eventlog <-
     log_db$find(sort = '{"_id": -1}', limit = last_n)

   eventlog <-
      eventlog[order(1:NROW(eventlog), decreasing = TRUE), ]

  } else {

    eventlog <- log_db$find()

  }

  eventlog$event_id <- paste0(eventlog$session_id,'#',eventlog$event_counter)

  event_params <- eventlog$event_params

  event_params <- Map(function(param) {

    null_params <- vapply(param, is.null, FUN.VALUE = logical(1))

    if (any(null_params)) {

      param[null_params] <- NA

    }

    unlist(param)

  }, event_params)

  event_params <- as.data.frame(event_params, stringsAsFactors = FALSE)

  eventlog <- cbind(eventlog, event_params)

  eventlog$event_params <- NULL

  eventlog

} # end of read_log

