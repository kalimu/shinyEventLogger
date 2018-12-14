#' @export

run_demo <- function(in_background = FALSE) {

  app_path <- system.file("shiny", "demoapp", package = "shinyEventLogger")

  if (in_background) {

    system("R -e \"shiny::runApp('inst/shiny/demoapp', port = 5555)\"",
           wait = FALSE)

    message("The App is available on 127.0.0.1:5555")

  } else {

    shiny::runApp(app_path, display.mode = "normal")

  }

} # end of run_demo

#' @export

run_demo_dashboard <- function() {

  app_path <- system.file("shiny", "dashboardapp", package = "shinyEventLogger")

  shiny::runApp(app_path, display.mode = "normal")

} # end of run_demo
