library(shiny)
library(shinydashboard)

id <- "myFunction"
ns <- NS(id)

ui <- fluidPage(
  fluidRow(
    uiOutput(ns("funParams"))
    )
)

server <- function(input, output, session) {
  moduleServer(
    id,
    function(input, output, session) {
      output$funParams <- renderUI({
        tagList(
          checkboxInput(session$ns("more"), "More"),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", session$ns("more")),
            h3(">> MORE")
          ))
      })
    })
}

shinyApp(ui, server)