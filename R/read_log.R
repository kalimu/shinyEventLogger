#' @export
read_log <- function(file = "examples/_events.log") {

  events_data <- readLines(con = file)

  events_data

}
