#' Purging eventlog
#'
#' \code{purge_eventlog} removes obsolete event records based
#' on selected criteria.
#' Please be careful. If you do not back up your eventlog, purging
#' operation can be irreversible.
#'
#' @param file A character string. Path to a file log.
#' @param min_build An integer. Minimum build version of the app
#' that should be kept in the eventlog after purging.
#'
#' @importFrom stringr str_extract
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  demo_filelog <- system.file("shiny", "demoapp/events.log",
#'                              package = "shinyEventLogger")
#'
#'  temp_file <- tempfile()
#'  file_conn <- base::file(temp_file)
#'  writeLines(readLines(con = demo_filelog), file_conn)
#'  close(file_conn)
#'
#'  purge_eventlog(file = temp_file, min_build = 23)
#' }

purge_eventlog <- function(file      = NULL,
                           min_build = NULL
                           ) {

  if (is.null(min_build)) {

    stop("You need to specify purging criterium (eg. `min_build`)")

  }

  n_lines <- R.utils::countLines(file)

  build         <- -1
  lines_to_skip <- -1

  while (build < min_build)   {

    lines_to_skip <- lines_to_skip + 1

    if (lines_to_skip >= n_lines) break()

    eventlog_item <-
      scan(file,
           what = "",
           skip = lines_to_skip,
           nlines = 1,
           sep = "\n",
           quiet = TRUE)

    build <- stringr::str_extract(string = eventlog_item,
                                  pattern = "build = [0-9]{1,}")

    build <- as.integer(
      stringr::str_extract(string = build,
                           pattern = "[0-9]{1,}")
      )

  } # end of while

  if (lines_to_skip <= 0) {

    message("Nothing to do. ",
            "Build version found at the beginning of the eventlog file: ",
            build)

    return()

  } # end of if

  message("Removing eventlog records...")

  eventlog <-
    scan(file,
         what = "",
         skip = lines_to_skip,# n_lines - last_n,
         nlines = n_lines,
         sep = "\n",
         quiet = TRUE)

  file_conn <- base::file(file)

  writeLines(eventlog, file_conn)

  close(file_conn)

  message("Records removed: ", lines_to_skip)
  message("Records kept in the eventlog: ", n_lines - lines_to_skip)

} # end of purge_eventlog

