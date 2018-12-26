
read_eventlog_file <- function(file = getOption("shinyEventLogger.file"),
                               last_n = Inf,
                               verbose = TRUE
                               ) {

  if (!file.exists(file)) {

    message("Log file: '", file, "' DOES NOT exist.")
    message("Please check file name, file path,",
            "and current working directory: \n",
            getwd())

    return()

  } else if (verbose) {

    message("Reading log file: ", file)

  }

  if (last_n != Inf) {

    n_lines <- R.utils::countLines(file)

    eventlog <-
      scan(file,
           what = "",
           skip = n_lines - last_n,
           nlines = last_n,
           sep = "\n",
           quiet = !verbose)

  } else {

    eventlog  <- readLines(con = file)

  }

  eventlog  <- strsplit(eventlog , split = "|", fixed = TRUE)

  if (any(lapply(eventlog , length) != 9)) {

    stop("Some of the log records have unexpected length.")

  }

  eventlog <- data.frame(Reduce(base::rbind, eventlog),
                         stringsAsFactors = FALSE)

  eventlog[1] <- NULL
  names(eventlog) <-
    c("event_counter", "event_type", "event_name", "event_status",
      "event_params", "session_id", "event_timestamp", "event_body")

  eventlog$event_timestamp <-
    as.POSIXct(eventlog$event_timestamp)

  eventlog$event_id <- paste0(eventlog$session_id, eventlog$event_counter)

  eventlog$event_params <-
    ifelse(eventlog$event_params == "", NA, eventlog$event_params)

  event_params <- eventlog$event_params

  event_params <- purrr::map(event_params,
                             function(x) {
                               eval(base::parse(text = x))
                               }
                             )

  event_params <-
    purrr::map_dfr(event_params, function(x) {

      as.data.frame(x, stringsAsFactors = FALSE)

    })

  event_params$NA_character_. <- NULL

  eventlog <- cbind(eventlog, event_params)

  eventlog$event_params <- NULL

  if (!"resource" %in% names(eventlog)) {

    eventlog$resource <- eventlog$event_type

  }

  eventlog

} # end of read_log

