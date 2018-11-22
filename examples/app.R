

library(shiny)
library(shinyEventLogger)
devtools::load_all(".")

# Setting up different kinds of logging
set_logging(
  r_console = TRUE,
  js_console = TRUE,
  file = "events.log"
)



# Logging outside reactive session
log_event("Starting the app...", type = "NONREACTIVE")

ui <- fluidPage(

  # Initiate shinyLogger
  log_init(),

  titlePanel("Old Faithful Geyser Data"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = 30)
      ),

      mainPanel(
        plotOutput("distPlot"),
        tableOutput("events")
        )

    )
)

server <- function(input, output, session) {

  log_event("Starting the server function...")

  log_event("Logging done with shinyEventLogger",
           params = list(
             package_version = paste0(packageVersion("shinyEventLogger"),
                              collapse = ".")
                )

           )

  packageVersion("shinyEventLogger")

  rv <- reactiveValues(a = 36)

  output$events <- renderTable({

    # log_event("Rendering table", status = "STARTED")
    log_started("Rendering table")

    bins = input$bins

    Sys.sleep(2)

    # log_event("Rendering table", status = "DONE")
    log_done("Rendering table")

    bins


  })

   output$distPlot <- renderPlot({

      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # Logging character string
      if (input$bins > 40)
        log_event(name = "Number of bins more than 40",
                 input$bins)



      # Logging function output
      log_output(str(faithful))

      # Logging data.frame
      log_output(head(faithful))

      # Logging current value
      log_value(input$bins)

      # Logging unit tests
      log_test(testthat::expect_gte(object = input$bins , expected = 25))

      # Logging and rising a diagnostic message
      if (input$bins == 48) log_message("50 bins are comming!")

      # Logging and rising a warning
      if (input$bins == 49) log_warning("Very close to 50 bins!")

      # Logging and rising an error
      if (input$bins == 50) log_error("50 bins are not allowed!")

      log_output(rv$a)

      hist(x, breaks = bins, col = 'darkgray', border = 'white')

   })

   observe({

     input$bins

     cat(rv$a, "\n")
     log_value(rv$a)



   })

}

shinyApp(ui = ui, server = server)

