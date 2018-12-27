#' @export

purge_eventlog <- function(file      = NULL,
                           # TODO: db        = NULL,
                           # TODO: min_date  = NULL,
                           min_build = NULL#,
                           # TODO: last_n    = Inf,
                           # TODO: verbose   = TRUE
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

