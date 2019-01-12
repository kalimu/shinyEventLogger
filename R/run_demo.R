#' Run demo shiny app
#'
#' \code{run_demo} runs demo shiny app which logs different types of events.
#' \code{run_demo_dashboard} runs demo shiny dashboard that allows
#' for interactive analysis of events from demo app.
#' The demo app can be also run in background and events fired in the app
#' can be seen immediately in the demo dashboard.
#'
#' @param in_background A logical.
#'   If \code{TRUE} the demo shiny app is run in the background on port 5555.
#'   Default is \code{FALSE}.
#'
#' @describeIn run_demo Run demo shiny app
#'
#' @import shiny
#'
#' @export

run_demo <- function(in_background = FALSE) {

  app_path <- system.file("shiny", "demoapp", package = "shinyEventLogger")

  if (in_background) {

    system(
      paste0("R -e \"shiny::runApp('",
             app_path,
             "', port = 5555), display.mode = 'normal'\""),
      wait = FALSE
      )

    message("The App is available on 127.0.0.1:5555")
    browseURL("http://127.0.0.1:5555")

  } else {

    shiny::runApp(app_path, display.mode = "normal")

  }

} # end of run_demo

#' @describeIn run_demo Run demo shiny dashboard
#' @export

run_demo_dashboard <- function() {

  app_path <- system.file("shiny", "dashboardapp", package = "shinyEventLogger")

  shiny::runApp(app_path, display.mode = "normal")

} # end of run_demo
