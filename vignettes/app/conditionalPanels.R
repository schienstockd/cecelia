library(shiny)
library(shinydashboard)
library(sortable)

id <- "myFunction"
ns <- NS(id)

ui <- fluidPage(
  fluidRow(
    uiOutput(ns("funParams")),
    verbatimTextOutput(ns("results_basic"))
    )
)

server <- function(input, output, session) {
  moduleServer(
    id,
    function(input, output, session) {
      output$funParams <- renderUI({
        rank_list(
          text = "",
          labels = list(
            "q3qr" = tagList(
              checkboxInput(session$ns("one"), "ONE"),
              checkboxInput(session$ns("one_again"), "ONE another"),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", session$ns("one")),
                h3(">> ONE MORE")
              )),
            "rew23" = tagList(
              checkboxInput(session$ns("two"), "TWO"),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", session$ns("two")),
                h3(">> TWO MORE"),
                checkboxInput(session$ns("two_again"), "TWO another"),
                checkboxInput(session$ns("two_again_1"), "TWO again")
              ))
          ),
          input_id = session$ns("ranks")
        )
      })
      
      output$results_basic <- renderPrint({
        # sapply(input$ranks, function(x) stringr::str_split(x, "\n")[[1]][[1]], USE.NAMES = FALSE)
        input$ranks
      })
    }
  )
}

shinyApp(ui, server)