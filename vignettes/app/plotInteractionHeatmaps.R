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

# BEHAVIOUR XCR1-venus
# pID <- "8BR53W"
# versionID <- 1
# uID <- "0Oenks"

# Population interactions from clustering IMC
pID <- "pEdOoZ"
versionID <- 2
uID <- "U7LRc9"

id <- "plotInteractionHeatmaps"
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
      spatialDT <- reactiveVal()
      popDT <- reactiveVal()
      summaryDT <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      # DEBUG
      selectedUIDs <- reactive({
        req(expInfo())
        
        # expInfo()$uID
        
        # IMC
        sample(expInfo()[, Include == "Y", uID]$uID, size = 5)
        
        # MILAS SPLEEN
        # expInfo()[dpi == "1-5" & Genotype != "zDC"]$uID
        
        # # behaviour DTx
        # uIDs[!uIDs %in% c(
        #   "5N8Iip", "OWJrYz", "PxwhNn",
        #   "CzR7ZQ", "zqrpfq",
        #   "NbaQvC", "ypUN8d", "oPmJg0",
        #   "o0auGO", "TxTL0a"
        # )]
      })
      
      # value name
      valueName <- reactive({
        # DEBUG - need a selection box for this
        "default"
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
      
      # properties to show
      resultParamsCols <- reactive({
        input$resultParamsCols
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # summary properties to show
      resultSummaryAxisX <- reactive({
        input$resultSummaryAxisX
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      resultSummaryAxisY <- reactive({
        input$resultSummaryAxisY
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # plot properties
      plotWidth <- reactive({
        input$plotWidth
      })
      
      plotHeight <- reactive({
        input$plotHeight
      })
      
      # points data that is shown
      summaryData <- reactive({
        req(summaryDT())
        
        summaryDT()
      })
      
      # plot data that is shown
      plotData <- reactive({
        req(summaryPlotData())
        
        summaryPlotData()
      })
      
      # summary data for plot
      summaryPlotData <- reactive({
        req(summaryData())
        
        groupA <- paste0(resultSummaryAxisX(), ".from")
        groupB <- paste0(resultSummaryAxisX(), ".to")
        
        # generate plot layers
        p1 <- ggplot(data = summaryData(),
                     aes(x = as.factor(get(groupA)),
                         y = as.factor(get(groupB))))
        
        xlabTitle <- ""
        ylabTitle <- ""
        
        # main tiles
        p1 <- p1 + geom_tile(aes(fill = freq), colour = "white", size = 0.5) +
          viridis::scale_fill_viridis(
            limits = c(0, 100),
            breaks = c(0, 50, 100)
            # labels = c(0, 50, 100)
          )
        
        # format plot
        .formatSummaryPlotData(
          p1, input, xlabTitle = xlabTitle, ylabTitle = ylabTitle)
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
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
      
      # listen to image selection
      # observeEvent(moduleManagers()$selectionManager$selectedUIDs(), {
      observeEvent(c(
        selectedUIDs(),
        popType(),
        resultParamsPops()
        ), {
        # req(moduleManagers()$selectionManager$selectedUIDs())
        req(selectedUIDs())
        req(popType())
        req(resultParamsPops())
        
        # make sure that it only requests data when selected
        req(globalManagers$input$sidebar() == session$ns(c()))
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        # get population data
        DT <- cciaSet()$popDT(
          popType = popType(),
          uIDs = selectedUIDs(),
          includeFiltered = TRUE,
          completeDT = FALSE,
          replaceNA = TRUE,
          pops = resultParamsPops(),
          popCols = c("label", "pop")
        )
        
        # get interaction data
        sDT <- cciaSet()$spatialDT(
          valueName = valueName(), uIDs = selectedUIDs())
        
        # join pops
        sDT[DT[, c("uID", "label", "pop")],
            on = c("uID", "to" = "label"),
            pop.to := pop]
        sDT[DT[, c("uID", "label", "pop")],
            on = c("uID", "from" = "label"),
            pop.from := pop]
        sDT[DT[, c("uID", "label", "clusters")],
            on = c("uID", "to" = "label"),
            clusters.to := clusters]
        sDT[DT[, c("uID", "label", "clusters")],
            on = c("uID", "from" = "label"),
            clusters.from := clusters]
        # exclude self interactions
        # sDT <- sDT[clusters.to != clusters.from]
        
        progress$close()
        
        popDT(DT)
        spatialDT(sDT)
      })
      
      # create summary DT
      observeEvent(c(popDT(), resultSummaryAxisX(), resultSummaryAxisY()), {
        req(nrow(popDT()) > 0)
        req(resultSummaryAxisX())
        req(resultSummaryAxisY())
        
        groupA <- paste0(resultSummaryAxisX(), ".from")
        groupB <- paste0(resultSummaryAxisX(), ".to")
        
        summaryDT(as.data.table(
          spatialDT() %>%
            # group_by(.dots = c("uID", groupA, groupB)) %>%
            group_by(.dots = c(groupA, groupB)) %>%
            summarise(n = n()) %>%
            mutate(freq = n/sum(n) * 100) %>%
            drop_na() %>%
            ungroup() %>%
            # TODO is there a better way?
            # https://stackoverflow.com/a/67082584
            # complete(uID, !!!syms(groupA), !!!syms(groupB), fill = list(freq = 0))
            complete(!!!syms(groupA), !!!syms(groupB), fill = list(freq = 0))
        ))
      })
        
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
        popTypeCols <- list()
        popCats <- list()
        
        if (!is.null(popType())) {
          popTypePops <- unname(cciaSet()$popPaths(
            uIDs = selectedUIDs(), popType = popType(),
            includeFiltered = TRUE, filterMeasures = c("clusters")))
        }
        
        # get choices for categories
        propCols <- list(
          "Raw interactions" = "raw"
        )
        
        # add pop and clustering to X-axis
        if (length(popDT()) > 0) {
          if ("clusters" %in% colnames(popDT()))
            popTypeCols <- c("clusters", popTypeCols)
          if ("pop" %in% colnames(popDT()))
            popTypeCols <- c("pop", popTypeCols)
        }
        
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
              # selected = "live"
              selected = "clust"
            ),
            createSelectInput(
              session$ns("resultParamsPops"),
              label = "Populations to get",
              choices = popTypePops,
              multiple = TRUE,
              # selected = isolate(resultParamsPops())
              # selected = c("OTI/tracked", "gBT/tracked")
              # selected = c("gBT+", "gBT+/clustered")
              # selected = c("tcells.gBT/tracked", "dcs.all/tracked")
              selected = if (is.null(popType()))
                c("non.debris")
              else
                unname(cciaSet()$popPaths(
                  uIDs = selectedUIDs(), popType = popType(),
                  includeFiltered = TRUE, filterMeasures = c("clusters")))
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
            tags$label("Summary plots"),
            createSelectInput(
              session$ns("resultSummaryAxisX"),
              label = "X Axis",
              choices = popTypeCols,
              multiple = FALSE,
              selected = isolate(resultSummaryAxisX())
              # selected = c(
              #   "live.cell.hmm.state.shape", "live.cell.hmm.state.movement")
            ),
            createSelectInput(
              session$ns("resultSummaryAxisY"),
              label = "Y Axis",
              choices = propCols,
              multiple = FALSE,
              selected = isolate(resultSummaryAxisY())
            )
          )
        ))
      })
      
      # plot params
      output$plotParams <- renderUI({
        tagList(
          h4("Plot Layout"),      
          
          checkboxInput(session$ns("rotatePlot"),
                        label = "Rotate plot 90 degrees",
                        value = FALSE),
          
          checkboxInput(session$ns("rotateXLabel"),
                        label = "Rotate X axis 45 degrees",
                        value = TRUE),
          
          checkboxInput(session$ns("noGrid"),
                        label = "Remove gridlines",
                        value = TRUE),
          
          checkboxInput(session$ns("changeScale"),
                        label = "Change scale",
                        value = FALSE),
          conditionalPanel(condition = sprintf("input['%s'] == true", session$ns("changeScale")),
                           textInput(session$ns("range"), "Range of values (min,max)", value = "")),
          
          checkboxInput(session$ns("darkTheme"), label = "Dark Theme", value = FALSE),
          numericInput(session$ns("plotHeight"), "Height (# pixels): ", value = 480),
          numericInput(session$ns("plotWidth"), "Width (# pixels):", value = 480),
          
          h4("Labels/captions"),
          
          checkboxInput(session$ns("addTitle"),
                        label = "Add title",
                        value = FALSE),
                        
          conditionalPanel(
            condition = sprintf("input['%s'] == true", session$ns("addTitle")),
            textInput(session$ns("title"), "Title:", value = "")
          ),
          
          checkboxInput(session$ns("labelAxes"),
                        label = "Change labels",
                        value = FALSE),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == true", session$ns("labelAxes")),
            textInput(session$ns("labX"), "X-axis:", value = ""),
            textInput(session$ns("labY"), "Y-axis:", value = "")),
          
          checkboxInput(session$ns("adjFontSize"),
                        label = "Change font size",
                        value = FALSE),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == true", session$ns("adjFontSize")),
            numericInput(session$ns("adjFontSizeAxTitle"), "Size axis titles:", value = 24),
            numericInput(session$ns("adjFontSizeAxLabels"), "Size axis labels:", value = 18)),
          checkboxInput(session$ns("addDescription"),
                        label = "Add figure description",
                        value = TRUE),
          
          checkboxInput(session$ns("showFacetTitles"),
                        label = "Show facet titles",
                        value = TRUE)
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
