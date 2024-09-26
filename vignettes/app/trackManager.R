library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

library(flowCore)
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
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

# 2P tutorial
pID <- "Co3HDh"
versionID <- 1
uID <- "QnkxJE"

id <- "celltrackManager"
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
      popDT <- reactiveVal()
      
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
      
      # DEBUG
      selectedUIDs <- reactive({
        req(expInfo())
        
        expInfo()$uID
        
        # MILAS SPLEEN
        # expInfo()[dpi == "1-5" & Genotype != "zDC"]$uID
        
        # Arm Spleen
        c("lWinrY")
        
        # # behaviour DTx
        # uIDs[!uIDs %in% c(
        #   "5N8Iip", "OWJrYz", "PxwhNn",
        #   "CzR7ZQ", "zqrpfq",
        #   "NbaQvC", "ypUN8d", "oPmJg0",
        #   "o0auGO", "TxTL0a"
        # )]
      })
      
      # pop type
      popType <- reactive({
        "live"
      })
      
      # populations
      popsTracked <- reactive({
        popsList <- cciaSet()$popPaths(popType = popType(), includeFiltered = TRUE, uIDs = selectedUIDs())
        
        popsList[!is.na(stringr::str_match(popsList, "/tracked$"))]
      })
      
      # experimental info
      expInfo <- reactive({
        req(cciaSet())
        
        as.data.table(cciaSet()$summary(withSelf = FALSE, fields = c("Attr")))
      })
      
      ## Generic
      
      # listen to image selection
      # observeEvent(moduleManagers()$selectionManager$selectedUIDs(), {
      observeEvent(c(
        selectedUIDs(),
        popType()
      ), {
        # req(moduleManagers()$selectionManager$selectedUIDs())
        req(selectedUIDs())
        req(popType())
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        # get popDT
        # create tracks from populations?
        
        # popDT(moduleManagers()$imageSetManager$selectedSet()$popDT(
        DT <- cciaSet()$popDT(
          popType = popType(),
          uIDs = selectedUIDs(),
          includeFiltered = TRUE,
          completeDT = TRUE,
          replaceNA = TRUE,
          pops = popsTracked()
        )
        
        progress$close()
        
        popDT(DT)
      })
      
      ### UI Outputs
      ## Tables
      
      ## Plots
      output$trackTraces <- renderPlotly({
        req(popDT())
        
        browser()
        
        colourPalette <- randomcoloR::distinctColorPalette(
          length(unique(popDT()[value_name == "OTI"]$track_id)))
        
        # make track ids categorical
        # make lines between dots
        # show labels as hover information
        # plot tracks on the right
        # somehow make them selectable
        
        p1 <- plot_ly(
          popDT()[value_name == "OTI"],
          x = ~track_id, y = ~-centroid_t,
          type = "scatter", mode = "markers", source = "trackTraces",
          # https://stackoverflow.com/questions/39859647/set-marker-size-in-plotly
          # marker = list(sizeref = 0.1, sizemode="area")
          # marker = list(size = 5),
          marker = list(size = 10),
          # size = 0.01,
          color = ~track_id,
          colors = colourPalette
          ) %>%
          layout(
            legend = list(
              title = list(text = NULL),
              # show entries horizontally
              orientation = "h",  
              itemsizing = "constant"
              # use center of legend as anchor
              # xanchor = "center", 
              # x = 0.5
            ),
            dragmode = "pan",
            plot_bgcolor = "#222",
            paper_bgcolor = "#222",
            font = list(color = 'white'),
            xaxis = plotlyAx,
            yaxis = plotlyAx
          ) 
        
        p1 %>%
          toWebGL()
      })
      
      output$plotOutput <- renderUI({
        tagList(
          fluidRow(plotlyOutput(ns("trackTraces"), height = "1200px"))
        )
      })
      
      ## Other
    }
  )
}

shinyApp(ui = ui, server = server)
