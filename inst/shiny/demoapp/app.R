library(shiny)
library(shinyEventLogger)

ui <- fluidPage(

  # Initiate shinyEventLogger (JavaScripts)
  log_init(),

  titlePanel("ShinyEventLogger: DEMO APP"),

  sidebarLayout(

    sidebarPanel(width = 3,

      selectInput("dataset", "Dataset:",
                  choices = c("faithful", "mtcars", "iris", "random"),
                  selected = "iris"),

      selectInput("variable", "Variable:",
                  choices = ""),

      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = 10)

    ),

    mainPanel(

      tabsetPanel(type = "pills",

        tabPanel(title = "Histogram", plotOutput("histogram")),

        tabPanel(title = "Last N Events", tableOutput("last_events"))

      )
    )
  )
) # end of ui

server <- function(input, output, session) {


  set_logging(
    # Setting up different kinds of logging
    r_console = TRUE,
    js_console = TRUE,
    file = "events.log",

    # Adding global parameters to all events
    logger_ver = as.character(packageVersion("ShinyEventLogger")),
    build = 015L
    )

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

        dataset <- data.frame("RandomValue" = rnorm(n = 50000000))

      } else {

        dataset <- eval(base::parse(text = input$dataset))

      }

    # Stopping timing the event
    log_done("Loading dataset")

    # Logging a value of number of rows
    log_value(NROW(dataset), params = list(n_rows = NROW(dataset)))

    # Logging function output
    log_output(str(dataset))

    # Logging data.frame
    log_output(head(dataset))

    dataset

  })

  observeEvent(input$dataset, {

        log_params(resource = "observe:input$dataset",
               fun = "observeEvent",
               dataset = input$dataset)

    # Logging arbitratry named event with value in output
    log_event(input$dataset, name = "Dataset was selected")
    # Logging the same value using deparsed expression as event name
    log_value(input$dataset)

    updateSelectInput(session, "variable",
                      choices = names(dataset()))

  })

  observeEvent(input$variable, {

      log_params(resource = "observe:input$variable",
                 fun = "observeEvent",
                 dataset = input$dataset)

      log_event(input$variable, name = "Variable was selected")
      log_value(input$variable)

  })

  output$histogram <- renderPlot(height = 600, {

    req(input$variable)
    req(input$bins)

    log_params(resource = "output$histogram",
               fun = "renderPlot",
               dataset = input$dataset)

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

    log_event("Plotting histogram")

    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white',
         main = paste0("Histogram of ", input$variable)
         )

   })

   observe({

     log_params(resource = "input$bins",
                fun = "observe",
                dataset = input$dataset)

     # Logging current input value
     log_value(input$bins)

     # Logging conditional named event
     if (input$bins < 20)
       log_event(name = "Number of bins are safe", input$bins)

     # Logging and rising a diagnostic message
     if (input$bins >= 30 & input$bins < 40)
       log_message("50 bins are comming...")

     # Logging and rising a non-critical warning
     if (input$bins >= 40 & input$bins < 50)
       log_warning("Very close to 50 bins!")

     # Logging and rising a critical error
     if (input$bins == 50) log_error("50 bins are not allowed!")

   })

  output$last_events <- renderTable({

    invalidateLater(2000)

    data <- read_eventlog(
      last_n = 25,
      verbose = FALSE,
      file = system.file("shiny", "demoapp/events.log",
                         package = "shinyEventLogger")
      )

    data <- data[, c("event_id", "event_type", "event",
                     "event_status", "output",
                     "dataset", "fun", "resource", "build", "logger_ver")]

    # trimming the output length
    output_length <- 12
    data$output <-
      ifelse(nchar(data$output) <= output_length, data$output,
             paste(strtrim(data$output, width = output_length - 5), "[...]")
             )

    # removing session id from event id
    data$event_id <-
      gsub(data$event_id, pattern = "^.*#", replacement =  "")

    data

  })

} # end of server

shinyApp(ui = ui, server = server)

