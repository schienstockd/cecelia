# https://stackoverflow.com/a/55530356
# To remove modules:
# https://roh.engineering/posts/2020/02/shiny-add/removing-modules-dynamically/

library(shiny)

row_ui <- function(id) {
  ns <- NS(id)
  
  tags$div(id = environment(ns)[['namespace']],
    fluidRow(
      column(3, 
             selectInput(ns("type_chooser"), 
                         label = "Choose Type:", 
                         choices = c("text", "numeric"))
      ),
      column(7, uiOutput(ns("ui_placeholder"))),
      column(2, actionButton(
        ns('deleteButton'), '', icon = shiny::icon('times'), style = 'float: right'))
    )
  )
} 

row_server <- function(input, output, session) {
  return_value <- reactive({input$inner_element})
  ns <- session$ns
  
  output$ui_placeholder <- renderUI({
    type <- req(input$type_chooser)
    if(type == "text") {
      textInput(ns("inner_element"), "Text:")
    } else if (type == "numeric") {
      numericInput(ns("inner_element"), "Value:", 0)
    }
  })
  
  ## if we later want to do some more sophisticated logic
  ## we can add reactives to this list
  list(return_value = return_value) 
}

remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

ui <- fluidPage(  
  div(id="placeholder"),
  actionButton("addLine", "Add Line"),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {
  handler <- reactiveVal(list())
  
  observeEvent(input$addLine, {
    new_id <- paste("row", input$addLine, sep = "_")
    insertUI(
      selector = "#placeholder",
      where = "beforeBegin",
      ui = row_ui(new_id)
    )
    
    handler_list <- isolate(handler())
    new_handler <- callModule(row_server, new_id)
    handler_list <- c(handler_list, new_handler)
    names(handler_list)[length(handler_list)] <- new_id
    handler(handler_list)
    
    observeEvent(input[[paste0(new_id, '-deleteButton')]], {
      removeUI(selector = sprintf('#%s', new_id))
      remove_shiny_inputs(new_id, input)
    })
  })
  
  output$out <- renderPrint({
    lapply(handler(), function(handle) {
      handle()
    })
  })
}

shinyApp(ui, server)