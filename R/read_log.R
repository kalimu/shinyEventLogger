#' @export

read_log <- function(file = getOption("shinyEventLogger_file")) {

  if (!file.exists(file)) {

    message("Log file: '", file, "' DOES NOT exist.")
    message("Please check current working directory: \n", getwd())
    return()

  } else {

    message("Reading log file: ", file)

  }

  datalog <- readLines(con = file)
  datalog <- strsplit(datalog, split = "|", fixed = TRUE)

  if (any(lapply(datalog, length) != 7)) {

    stop("Some of the log records have unexpected length.")

  }

  datalog <- data.frame(Reduce(base::rbind, datalog),
                        stringsAsFactors = FALSE)

  names(datalog) <-
    c("event_id", "event_type", "event_status",
      "event", "session_id", "timestamp", "output")

  datalog$timestamp <-
    as.POSIXct(as.numeric(as.character(datalog$timestamp)),
               origin = "1970-01-01")

  datalog$event_id <- paste0(datalog$session_id, datalog$event_id)

  datalog <-
    bupaR::eventlog(
      eventlog = datalog,
      case_id = 'session_id',
      activity_id = 'event',
      activity_instance_id = 'event_id',
      lifecycle_id = 'event_status',
      timestamp = 'timestamp',
      resource_id = 'event_type',
      order = "auto"
      )

} # end of read_log

