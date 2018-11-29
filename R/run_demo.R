#' @export

run_demo <- function() {

  app_path <- system.file("shiny", "demoapp", package = "shinyEventLogger")

  shiny::runApp(app_path, display.mode = "normal")

} # end of run_demo
