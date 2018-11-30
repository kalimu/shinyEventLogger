

library(shiny)
library(shinyEventLogger)

devtools::load_all(".")

set_logging(

  # Setting up different kinds of logging
  r_console = TRUE,
  js_console = TRUE,
  file = "events.log",

  # Adding global parameters to all events
  logger_ver = as.character(packageVersion("ShinyEventLogger")),
  build = 008
)

ui <- fluidPage(

  # Initiate shinyEventLogger JavaScripts
  log_init(),

  titlePanel("ShinyEventLogger DEMO"),

  sidebarLayout(

    sidebarPanel(width = 3,

      selectInput("dataset", "Dataset:",
                  choices = c("faithful", "mtcars", "iris", "random"),
                  selected = "faithful"),

      selectInput("variable", "Variable:",
                  choices = ""),

      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = 10)

    ),

    mainPanel(

      plotOutput("histogram"),
      verbatimTextOutput("debugging")

    )
  )
) # end of ui

server <- function(input, output, session) {

  log_event("App (re)started")

  dataset <- reactive({

    req(input$dataset)

    # Setting local logging parameters
    log_params(resource = "dataset()",
               fun      = "reactive",
               dataset  = input$dataset)

    # Starting timing the event
    log_started("Loading dataset")

      if (input$dataset == "random") {

        dataset <- data.frame("RandomValue" = rnorm(n = 10000000))

      } else {

        dataset <- eval(base::parse(text = input$dataset))

      }

    # Stopping timing the event
    log_done("Loading dataset")

    # Logging a value of n rows
    log_value(NROW(dataset))

    # Logging function output
    log_output(str(dataset))

    # Logging data.frame
    log_output(head(dataset))

    dataset

  })

  observeEvent(input$dataset, {

    # Logging arbitratry event
    log_event(input$dataset, name = "Dataset was selected")

    updateSelectInput(session, "variable",
                      choices = names(dataset()))

  })

  output$histogram <- renderPlot({

    req(input$variable)
    req(input$bins)

    log_params(resource = "output$histogram", fun = "renderPlot")

    # Debugging the error:
      # Error in [.data.frame: undefined columns selected
      # while changing datasets

      # log_value(names(dataset()))
      # log_value(input$variable)

    # Fixing the error
    req(input$variable %in% names(dataset()))

    x <- dataset()[, input$variable]

    # Logging inside-app unit test
    # This one logs silent error when variable Species from iris is selected.
    log_test(testthat::expect_is(x, "numeric"))

    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white',
         main = paste0("Histogram of ", input$variable)
         )

   })


   observe({

     log_params(resource = "input$bins", fun = "observe")

     # Logging current input value
     log_value(input$bins)

     # Logging conditional named event
      if (input$bins < 20) log_event(name = "Number of bins are safe",
                                     input$bins)

     # Logging and rising a diagnostic message
      if (input$bins >= 30) log_message("50 bins are comming...")

      # Logging and rising a warning
      if (input$bins >= 40) log_warning("Very close to 50 bins!")

      # Logging and rising an error
      if (input$bins == 50) log_error("50 bins are not allowed!")

   })


  # output$events <- renderTable({
  #
  #   log_params(resource = "output$events",
  #              fun = "renderTable",
  #              dataset = input$dataset)
  #
  #   log_started("Rendering table")
  #
  #     Sys.sleep(2)
  #     tab <- dataset()
  #
  #   log_done("Rendering table")
  #
  #   tab
  #
  #
  # })

} # end of server

shinyApp(ui = ui, server = server)

