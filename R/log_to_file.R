
log_to_file <- function(...) {

args <- list(...)
event_to_log <- paste0(args, collapse = " ")
write(x = event_to_log, file = "_events.log", append = TRUE)


}