

log_to_r_console <- function(header,
                             body = ""
                             ) {

  if (missing(header)) stop("A header of log entry is missing.")

  message(header)

  if (body != "") {

    cat(file = stdout(),
        body)

  } # end of if

  return(
    paste0(header,
           ifelse(body != "", paste0("\n", body), "")
    )
  )

} # end of log_to_r_console()