#' @export

run_demo <- function() {

  app_path <- system.file("shiny", "demoapp", package = "shinyEventLogger")

  shiny::runApp(app_path, display.mode = "normal")

} # end of run_demo

#' @export

run_demo_dashboard <- function() {

  app_path <- system.file("shiny", "dashboardapp", package = "shinyEventLogger")

  shiny::runApp(app_path, display.mode = "normal")

} # end of run_demo
