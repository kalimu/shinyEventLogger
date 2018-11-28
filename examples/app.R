

library(shiny)
library(shinyEventLogger)

devtools::load_all(".")

# Setting up different kinds of logging
set_logging(
  r_console = TRUE,
  js_console = TRUE,
  file = "events.log",

  logger_ver = as.character(packageVersion("ShinyEventLogger")),
  build = 004
)

ui <- fluidPage(

  # Initiate shinyEventLogger
  log_init(),

  titlePanel("ShinyEventLogger DEMO"),

  sidebarLayout(

    sidebarPanel(width = 3,

      selectInput("dataset", "Select dataset:",
                  choices = c("faithful", "mtcars", "iris")
                  ),




      sliderInput("bins", "Number of bins:",
                  min = 1, max = 50, value = 30)

    ), # end of sidebarPanel

    mainPanel(

      plotOutput("distPlot"),
      tableOutput("events")

    ) # end of mainPanel
  ) # end of sidebarLayout
) # end of FLuidPage

server <- function(input, output, session) {

  # log_params(resource = "server")

  # log_event("Shiny server started")

  # log_event("Logging done with shinyEventLogger",
  #          params = list(
  #            package_version = paste0(packageVersion("shinyEventLogger"),
  #                             collapse = ".")
  #               )
  #
  #          )

  # packageVersion("shinyEventLogger")

  rv <- reactiveValues(a = 36)

  dataset <- reactive({

    log_params(resource = "dataset()",
               fun = "reactive",
               dataset = input$dataset)

    log_started("Loading dataset")

    dataset <- eval(base::parse(text = input$dataset))

    log_done("Loading dataset")

    log_value(NROW(dataset), params = list(a = 'aaaa'))

    dataset

  })


  output$events <- renderTable({

    log_params(resource = "output$events",
               fun = "renderTable",
               dataset = input$dataset)

    log_started("Rendering table")

      Sys.sleep(2)
      tab <- dataset()

    log_done("Rendering table")

    tab


  })

  output$distPlot <- renderPlot({

    log_params(resource = "output$distPlot", fun = "renderPlot")


      x    <- dataset()[, 2]
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

     log_params(resource = "input$bins", fun = "observe")

     input$bins

     log_value(rv$a)



   })

   # log_event("Shiny server finished")

}

shinyApp(ui = ui, server = server)

