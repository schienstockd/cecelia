library(shiny)
library(sortable)

ui <- fluidPage(
  
  
  
  splitLayout(
    radioButtons("rb", "Choose one:",
                 choiceNames = list(
                   icon("calendar"),
                   HTML("<p style='color:red;'>Red Text</p>"),
                   "Normal text"
                 ),
                 choiceValues = list(
                   "icon", "html", "text"
                 ),
                 selected = "text"
    ),
    textOutput("txt"),
    actionButton("change_radio","Change radio button selection")
  ),
  uiOutput("sortable"),
  actionButton("change_bucket", "Change bucket selection")
)


server <- function(input, output, session) {
  output$txt <- renderText({
    paste("You chose", input$rb)
  })
  
  rv <- reactiveValues(
    labels_start = c("a", "bb", "ccc"),
    labels_end = NULL
  )
  
  output$sortable <- renderUI({
    bucket_list(
      header = "This is a bucket list. You can drag items between the lists.",
      add_rank_list(
        input_id = "start_bucket",
        text = "Drag from here",
        labels = rv$labels_start
      ),
      add_rank_list(
        input_id = "end_bucket",
        text = "to here",
        labels = rv$labels_end
      )
    )
    
  })
  
  observeEvent(input$change_radio,{
    updateSelectInput(session, "rb", selected = "html")
    
  })
  
  observeEvent(input$change_bucket,{
    rv$labels_start <- setdiff(input$start_bucket, "ccc")
    rv$labels_end <- c(setdiff(input$end_bucket, "ccc"), "ccc")
  })
  
}

shinyApp(ui, server)
