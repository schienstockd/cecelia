# add rows to data
# https://community.rstudio.com/t/dynamic-widgets-in-shiny/161836/2
# for whole modules to be added
# https://roh.engineering/posts/2020/02/shiny-add/removing-modules-dynamically/

library(shiny)

excel_Input <- structure(list(number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                              words = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"),
                              company = c("First Company", "Second Company", "Third Company", "Fourth Company", 
                                          "Fifth Company", "Sixth Company", "Seventh Company", "Eighth Company", 
                                          "Ninth Company", "Tenth Company")
),
row.names = c(NA,10L), 
class = "data.frame")

ui <- fluidPage(
  uiOutput("fcompany"),
  uiOutput("add_exp"),
  actionButton("add_button", "Add")
)

server <- function(input, output, session) {
  
  output$fcompany <- renderUI({
    column(width = 2, textInput("id_zero", label = "Gra", placeholder = "Col"), 
           numericInput("idexp_zero", label = "", value = 4, min = 0, max = 10))
  })
  
  # reactive value to "collect" all widgets
  widget <- reactiveValues(collection = NULL)
  
  # reactive value to "collect" company inputs
  company <<- reactiveValues(inputs = list())
  
  # reactive val to count clicks
  click_count <- reactiveVal(1)
  
  # click "Add" button to, 
  # 1) track companies entered 
  # 2) update the widget collection
  # 3) update click counts
  observeEvent(input$add_button,{
    
    # update company inputs
    lapply(1:(click_count()), function(i){
      out = ''
      value = paste0('input$id_', excel_Input$words[excel_Input$number == i])
      if(!is.null(eval(parse(text = value)))) { out = eval(parse(text = value)) }
      company$inputs[i] <<- out
    })
    
    # function to create widget
    create_widget = function(i){
      column(width = 2,
             textInput(paste0("id_",excel_Input$words[excel_Input$number == i]),
                       label = excel_Input$company[excel_Input$number == i], 
                       value = company$inputs[i],
                       placeholder = paste0("Enter the ", excel_Input$company[excel_Input$number == i])),
             
             numericInput(paste0("idexp_",excel_Input$words[excel_Input$number == i]),
                          label = "",
                          value = 4, min = 0, max = 10)
      )
    }
    
    # update widget collection
    widget$collection <<- lapply(1:click_count(), create_widget)
    
    # update click count
    click_count(click_count() + 1)
    
  })
  
  # render the widget collection
  output$add_exp <- renderUI({
    widget$collection
  })
  
}

shinyApp(ui, server)
