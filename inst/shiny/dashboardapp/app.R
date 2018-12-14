library(shiny)
library(shinyEventLogger)

library(dplyr)
library(DiagrammeR)

library(bupaR)
library(processmapR)

group_activites <- function(data, act = c("unite", "collapse")) {

  act_type <-
    if (act[1] == "unite") {
      act_unite
    } else if (act[1] == "collapse") {
      act_collapse
    }

  data %>%
    act_type(
      "Bins selected" =
        c("Very close to 50 bins!", "50 bins are comming...",
          "Number of bins are safe",
          "testthat::expect_lt(input$bins, 50)",
          "50 bins are not allowed!",
          "input$bins"),
      "Dataset selected" =
        c("NROW(dataset)", "head(dataset)", "input$dataset", "str(dataset)",
          "Loading dataset",
          "Dataset was selected"),
      "Variable selected" =
        c("input$variable", "Variable was selected"),
      "Plotting histogram" =
        c("testthat::expect_is(x, `numeric`)", "Plotting histogram")
      )

} # end of group_activities()

# UI ##########################################################################
ui <- fluidPage(

  titlePanel("ShinyEventLogger: EVENT DASHBOARD"),

  sidebarLayout(

    sidebarPanel(width = 3,

      uiOutput("eventlog_summary"),
      uiOutput("eventlog_summary_filtered"),
      actionButton(inputId = "reload_eventlog",
                   label = "Reload eventlog")

    ),

    mainPanel(

      tabsetPanel(type = "pills",

        tabPanel(title = "Last N Events",
                 br(), tableOutput("last_events")),

        tabPanel(title = "Process map",
                 br(), uiOutput("process_map")),

        tabPanel(title = "Unit tests",
                 br(), uiOutput("unit_tests")),

        tabPanel(title = "Top traces",
                 plotOutput("top_traces")),

        tabPanel(title = "Time analysis",
                 uiOutput("time_analysis")),

        tabPanel(title = "Sequence analysis",
                 uiOutput("sequence_analysis")),

        tabPanel(title = "Resource analysis",
                 uiOutput("resource_analysis")),

        tabPanel(title = "Top users' actions",
                 uiOutput("top_users_actions"))

      )
    )
  )
) # end of ui

# SERVER ######################################################################
server <- function(input, output, session) {

  # last_events ---------------------------------------------------------------
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
    max_output_length <- 12

    data$output <- ifelse(
      nchar(data$output) <= max_output_length,
      data$output,
      paste(strtrim(data$output, width = max_output_length - 5), "[...]")
      )

    # removing session id from event id
    data$event_id <-
      gsub(data$event_id, pattern = "^.*#", replacement =  "")

    data

  })

  # eventlog_unfiltered ------------------------------------------------------
  eventlog_unfiltered <- reactive({

    input$reload_eventlog

    read_eventlog(
      verbose = FALSE,
      file = system.file("shiny", "demoapp/events.log",
                         package = "shinyEventLogger"))

  })

  # eventlog -----------------------------------------------------------------
  eventlog <- reactive({

    req(input$build_version)

    eventlog_unfiltered() %>%
      filter(between(build,
                     input$build_version[1],
                     input$build_version[2]))

  })

  # eventlog_summary ---------------------------------------------------------
  output$eventlog_summary <- renderUI({

    eventlog <- eventlog_unfiltered()
    min_build <- min(eventlog$build, na.rm = TRUE)
    max_build <- max(eventlog$build, na.rm = TRUE)

    tagList(

      p("There are ", strong(n_cases(eventlog)), " cases, ",
        strong(n_activities(eventlog)), " activities,", br(),
        " and ", strong(n_events(eventlog)), "events in the event log."),

      sliderInput(inputId = "build_version",
                  label = "Events from DemoApp build version",
                  min = min_build,
                  max = max_build,
                  value = c(max_build, max_build),
                  step = 1, ticks = FALSE))

  })

  # eventlog_summary_filtered ------------------------------------------------
  output$eventlog_summary_filtered <- renderUI({

    eventlog <- eventlog()

    p("Currently, we are using data from ",
      strong(n_cases(eventlog)), " cases, ", br(),
      strong(n_activities(eventlog)), " activities,",
      " and ", strong(n_events(eventlog)), "events.")

  })

  # top_users_actions ---------------------------------------------------------
  output$top_users_actions <- renderUI({

    plot_height <- 250

    tagList(

      renderPlot(height = plot_height, {

        data <-
          eventlog() %>%
          filter(event == "Dataset was selected" & !is.na(dataset)) %>%
          count(dataset) %>%
          arrange(desc(n)) %>%
          mutate(n = if_else(dataset == "iris", n - n_cases(eventlog()), n))

        barplot(data$n,
                names.arg = data$dataset,
                col = 'darkgray',
                border = 'white',
                main = "Dataset most often selected*",
                sub = "(*) without iris dataset selected by default")

      }), hr(),

      renderPlot(height = plot_height, {

       data <-
          eventlog() %>%
          filter(event == "input$variable" & output != "") %>%
          filter(!is.na(dataset)) %>%
          count(output, dataset) %>%
          arrange(desc(n)) %>%
          mutate(
            n = if_else(output == "Sepal.Length", n - n_cases(eventlog()), n)
            )

        barplot(data$n,
                names.arg = paste0(data$output, " (", data$dataset, ")"),
                col = 'darkgray',
                border = 'white',
                main = "Variables most often selected*",
                sub = "(*) without Sepal.Length selected by default")

      }), hr(),

      renderPlot(height = plot_height, {

        data <-
          eventlog() %>%
          filter(event == "input$bins") %>%
          count(output) %>%
          mutate(output = as.integer(output)) %>%
          arrange(output) %>%
          mutate(n = if_else(output == 10, n - n_cases(eventlog()), n))

        barplot(data$n,
                names.arg = data$output,
                col = 'darkgray',
                border = 'white',
                main = "Number of bins most often selected*",
                sub = "(*) without 10 bins selected by default")

      }), hr(), br()

    )

  })

  # process_map ---------------------------------------------------------------
  output$process_map <- renderUI({

    tagList(

      p("Maximum time"),
      renderGrViz({

        eventlog() %>%
          group_activites(act = "collapse") %>%
          processmapR::process_map(
            type_edge = frequency(),
            type_nodes = performance(units = "secs", FUN = max)
            )

      }),

      p("Mean time"),
      renderGrViz({

        eventlog() %>%
          group_activites(act = "collapse") %>%
          processmapR::process_map(
            type_edge = frequency(),
            type_nodes = performance(units = "secs", FUN = mean)
            )

      })

    )
  })

  # unit_tests ---------------------------------------------------------------
  output$unit_tests <- renderTable({

    eventlog() %>%
      filter(event_type == "TEST") %>%
      count(event_type, event, event_status, variable, bins, fun, resource) %>%
      arrange(event_status, event, variable, bins)

  })

  # top_traces ---------------------------------------------------------------
  output$top_traces <- renderPlot(height = 400, {

    eventlog() %>%
      # group_activites(act = "collapse") %>%
      group_activites(act = "unite") %>%
      # trace_explorer(coverage = 0.5)
      trace_explorer(coverage = 1)

  })

  # time_analysis -------------------------------------------------------------
  output$time_analysis <- renderUI({

    plot_height <- 350

    fluidPage(

      fluidRow(
        column(width = 6,

          br(), p("Throughput time per dataset"),
          renderPlot(height = plot_height, {

            eventlog() %>%
              filter(!is.na(dataset)) %>%
              group_by(dataset) %>%
              throughput_time(level = "log", units = "mins") %>%
              plot()

          })

        ),
        column(width = 6,

          br(), p("Idle time per dataset"),
          renderPlot(height = plot_height, {

            eventlog() %>%
              filter(!is.na(dataset)) %>%
              group_by(dataset) %>%
              idle_time(level = "log", units = "mins") %>%
              plot()

          })
        )
      ),

      fluidRow(
        column(width = 6, offset = 0,

          br(), p("Processing time per dataset"),
          renderPlot(height = plot_height, {

            eventlog() %>%
              filter(!is.na(dataset)) %>%
              group_by(dataset) %>%
              processing_time(level = "log", units = "mins") %>%
              plot()

          })

        ),
        column(width = 6, offset = 0,

          br(), p("Dotted chart of cases"),
          renderPlot(height = plot_height, {

            eventlog() %>%
              group_activites() %>%
              dotted_chart(units = "mins",
                           x = "relative",
                           sort = "duration")

          })

        )
      )
    )
  })

  # sequence_analysis --------------------------------------------------------
  output$sequence_analysis <- renderUI({

    plot_height <- 350

    fluidPage(
      fluidRow(
        column(width = 8, offset = 2,

          br(), p("Precedence matrix"),
          renderPlot(height = plot_height, {

            eventlog() %>%
              group_activites(act = "unite") %>%
              precedence_matrix(type = "relative") %>%
              plot()

          })

        )
      ),

      fluidRow(
        column(width = 6,

          br(), p("Repetitions"),
          renderPlot(height = plot_height * 0.6, {

            eventlog() %>%
              group_activites(act = "unite") %>%
              number_of_repetitions(level = "activity") %>%
              plot()

          })
        ),
        column(width = 6,

          br(), p("Selfloops"),
          renderPlot(height = plot_height * 0.6, {

            eventlog() %>%
              group_activites(act = "unite") %>%
              number_of_selfloops(level = "activity") %>%
              plot()

          })
        ),
        column(width = 6,

          br(), p("End activities (grouped)"),
          renderPlot(height = plot_height * 0.4, {

            eventlog() %>%
              group_activites(act = "unite") %>%
              end_activities(level = "activity") %>%
              plot()

          })
        ),
        column(width = 6,

          br(), p("End activities (ungrouped)"),
          renderPlot(height = plot_height * 0.4, {

            eventlog() %>%
              # group_activites(act = "unite") %>%
              end_activities(level = "activity") %>%
              plot()

          })
        )
      )
    )
  })

  # resource_analysis --------------------------------------------------------
  output$resource_analysis <- renderUI({

    plot_height = 600

    fluidRow(
      column(width = 6,

        br(),
        renderPlot(height = plot_height, {

          eventlog() %>%
            filter(!is.na(fun), !is.na(resource)) %>%
            group_by(fun) %>%
            resource_frequency(level = "resource") %>%
            plot() + ggplot2::theme(
              legend.position = "bottom",
              axis.title = ggplot2::element_blank()
              )

        })
      ),
      column(width = 6,

        br(), renderPlot(height = plot_height, {

          eventlog() %>%
            filter(!is.na(fun), !is.na(resource)) %>%
            group_by(fun) %>%
            resource_involvement(level = "resource") %>%
            plot() + ggplot2::theme(
              legend.position = "bottom",
              axis.title = ggplot2::element_blank()
              )
        })
      )
    )
  })

} # end of server

shinyApp(ui = ui, server = server)

