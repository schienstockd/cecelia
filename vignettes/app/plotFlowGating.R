library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

devtools::load_all("../../")
cciaUse("~/cecelia/dev")
# cciaUse("~/cecelia/dev", initConda = FALSE)

# source all files from subdirectories
# TODO is there a better way of doing this?
appSources <- c(
  file.path("..", "inst", "app", "constantsCore.R"),
  list.files(file.path("..", "inst", "app", "lib"), pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("..", "inst", "app", "helpers"), pattern = ".R$", recursive = TRUE, full.names = TRUE)
)

for (x in appSources) {
  source(x)
}

# confidence level
confidencePercentage = 95
confidenceLevel = confidencePercentage/100

# Population gating spleen
pID <- "pEdOoZ"
versionID <- 2
uID <- "ktUu0n"

id <- "plotFlowGating"
ns <- NS(id)

ui <- fluidPage(
  fluidRow(
    shinydashboard::box(
      id = ns("plotData"),
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Data selection",
      status = "primary",
      width = 12,
      uiOutput(ns("plotData"))
    )
  ),
  fluidRow(
    shinydashboard::box(
      id = ns("plots"),
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Plot output",
      status = "primary",
      width = 12,
      column(3, uiOutput(ns("plotParams"))),
      column(9, uiOutput(ns("plotOutput")))
    )
  )
)

server <- function(input, output, session) {
  moduleServer(
    id,
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      # DEBUG
      selectedUIDs <- reactive({
        req(expInfo())
        
        # expInfo()$uID
        c("TURHVv")
      })
      
      # pop type
      popType <- reactive({
        input$resultParamsPopType
      })
      
      # experimental info
      expInfo <- reactive({
        req(cciaSet())
        
        as.data.table(cciaSet()$summary(withSelf = FALSE, fields = c("Attr")))
      })
      
      # populations to get
      resultParamsPops <- reactive({
        input$resultParamsPops
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # plot properties
      plotWidth <- reactive({
        input$plotWidth
      })
      
      plotHeight <- reactive({
        input$plotHeight
      })
      
      # summary data for plot
      summaryPlotData <- reactive({
        req(cciaObj())
        req(resultParamsPops())
        req(all(
          !is.null(input$labelSize),
          !is.null(input$asContours),
          !is.null(input$showPopColours),
          !is.null(input$directLeaves),
          !is.null(input$nRow),
          !is.null(input$nCol)
        ))
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        p1s <- .flowPlotGatedRaster(
          cciaObj(),
          popPath = resultParamsPops(),
          labelSize = input$labelSize,
          asContours = input$asContours,
          showPopColours = input$showPopColours,
          directLeaves = input$directLeaves
          )
        
        progress$close()
        
        req(length(p1s) > 0)
        
        ggpubr::ggarrange(plotlist = p1s, nrow = input$nRow, ncol = input$nCol)
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # plot data that is shown
      plotData <- reactive({
        req(summaryPlotData())
        
        summaryPlotData()
      })
      
      # selected ccia object
      cciaObj <- reactive({
        # moduleManagers()$imageViewerManager$shownImage()
        initCciaObject(pID = pID, uID = selectedUIDs()[[1]], versionID = versionID)()
      })
      
      # selected ccia set
      cciaSet <- reactive({
        # moduleManagers()$imageSetManager$selectedSet()
        initCciaObject(pID = pID, uID = uID, versionID = versionID)()
      })
      
      ### Observers - RxAction
      ## Event specific
      
      ## Generic
      
      ### UI Outputs
      ## Tables
      
      ## Plots
      
      # plot data
      output$plotData <- renderUI({
        # req(popType())
        req(cciaObj())
        
        # get pop type columns
        popTypePops <- list()
        
        if (!is.null(popType())) {
          popTypePops <- unname(cciaSet()$popPaths(
            uIDs = selectedUIDs(), popType = popType(), includeFiltered = TRUE))
        }
        
        # get pop type columns
        popTypeChoices <- cciaConf()$parameters$popTypes
        
        # create ui elements
        tagList(fluidRow(
          column(
            3,
            tags$label("Parameter plots"),
            selectInput(
              session$ns("resultParamsPopType"), "Population Type",
              choices = .reverseNamedList(popTypeChoices),
              # selected = isolate(popType())
              selected = "flow"
            ),
            createSelectInput(
              session$ns("resultParamsPops"),
              label = "Populations to get",
              choices = popTypePops,
              multiple = FALSE,
              selected = isolate(resultParamsPops())
            )
            # createSelectInput(
            #   session$ns("resultParamsCols"),
            #   label = "Properties",
            #   choices = unname(cciaObj()$labelPropsCols()),
            #   multiple = TRUE,
            #   selected = isolate(resultParamsCols())
            # )
          ),
          column(
            3,
            tags$label("Summary plots")
          )
        ))
      })
      
      # plot params
      output$plotParams <- renderUI({
        tagList(
          h4("Gating"),      
          
          sliderInput(
            session$ns("labelSize"), label = "Label size",
            value = 2, min = 0.2, max = 10, step = 0.2
          ),
          sliderInput(
            session$ns("nRow"), label = "Rows",
            value = 2, min = 1, max = 10, step = 1
          ),
          sliderInput(
            session$ns("nCol"), label = "Columns",
            value = 2, min = 1, max = 10, step = 1
          ),
          checkboxInput(
            session$ns("showPopColours"), label = "Show pop colours", value = FALSE),
          checkboxInput(
            session$ns("asContours"), label = "Show contours", value = TRUE),
          checkboxInput(
            session$ns("directLeaves"), label = "Direct leaves", value = FALSE),
          
          h4("Plot Layout"),
          numericInput(session$ns("plotHeight"), "Height (# pixels): ", value = 480),
          numericInput(session$ns("plotWidth"), "Width (# pixels):", value = 480),
        )
      })
      
      # plot output
      output$plotOutput <- renderUI({
        tagList(
          fluidRow(
            downloadButton(ns("downloadPlotPDF"), "Download pdf-file"),
            downloadButton(ns("downloadPlotSVG"), "Download svg-file"), 
            downloadButton(ns("downloadPlotEPS"), "Download eps-file"), 
            downloadButton(ns("downloadPlotPNG"), "Download png-file"),
            downloadButton(ns("downloadPlotCSV"), "Download csv-file")
          ),
          br(),
          fluidRow(
            tabsetPanel(
              id = ns("plotOutputTabs"),
              selected = "combined",
              tabPanel(
                "Combined", value = "combined",
                plotOutput(ns("plotOutputCombined"), height = "400px")
              )
            )
          )
        )
      })
      
      # combined plots
      output$plotOutputCombined <- renderPlot(width = plotWidth, height = plotHeight, {
        req(summaryPlotData())
        
        plot(summaryPlotData())
      })
      
      # individual image plots
      output$plotOutputIndv <- renderPlot(width = plotWidth, height = plotHeight, {
        req(indvPlotData())
        
        plot(indvPlotData())
      })
      
      ## Buttons
      output$downloadPlotPDF <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".pdf", sep = "")
        },
        content <- function(file) {
          pdf(file, width = plotWidth()/72, height = plotHeight()/72)
          plot(plotData())
          dev.off()
        },
        contentType = "application/pdf" # MIME type of the file
      )
      
      output$downloadPlotSVG <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".svg", sep = "")
        },
        content <- function(file) {
          svg(file, width = plotWidth()/72, height = plotHeight()/72)
          plot(plotData())
          dev.off()
        },
        contentType = "application/svg" # MIME type of the file
      )
      
      output$downloadPlotEPS <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".eps", sep = "")
        },
        content <- function(file) {
          cairo_ps(file, width = plotWidth()/72, height = plotHeight()/72)
          plot(plotData())
          dev.off()
          
        },
        contentType = "application/eps" # MIME type of the file
      )
      
      output$downloadPlotPNG <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".png", sep = "")
        },
        content <- function(file) {
          png(file, width = plotWidth()*4, height = plotHeight()*4, res = 300)
          plot(plotData())
          dev.off()
        },
        contentType = "application/png" # MIME type of the file
      )
      
      output$downloadPlotCSV <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".csv", sep = "")
        },
        content <- function(file) {
          write.csv(summaryData(), file)
        },
        contentType = "text/csv" # MIME type of the file
      )
      
      ## Other
    }
  )
}

shinyApp(ui = ui, server = server)
