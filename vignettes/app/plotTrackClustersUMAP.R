library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

devtools::load_all("../../")
cciaUse("~/Desktop/DOHERTY/cecelia")
# cciaUse("~/Desktop/DOHERTY/cecelia", initConda = FALSE)

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
pID <- "8BR53W"
versionID <- 1
uID <- "0Oenks"

id <- "plotTrackClustersUMAP"
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
      summaryDT <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      # DEBUG
      selectedUIDs <- reactive({
        req(expInfo())
        
        expInfo()$uID
        
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
      
      # clustering to get
      resultParamsClustering <- reactive({
        input$resultParamsClustering
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
      pointsData <- reactive({
        req(popDT())
        
        popDT()
      })
      
      # summary data that is shown
      summaryData <- reactive({
        req(summaryDT())
        
        summaryDT()
      })
      
      # plot data that is shown
      plotData <- reactive({
        req(pointsData())
        
        pointsData()
      })
      
      # summary data for plot
      summaryPlotData <- reactive({
        req(pointsData())
        
        widthColumn <- 0.7
        
        # Change linecolor in case of dark mode
        if (input$darkTheme) {
          lineColor <- "grey80"
        } else if (input$darkTheme == FALSE) {
          lineColor <- "black"
        } 
        
        # generate plot layers
        p1 <- ggplot(data = pointsData(), aes(UMAP_1, UMAP_2))
        
        xlabTitle <- ""
        ylabTitle <- ""
        
        # main map
        p1 <- p1 + geom_point(aes(color = get(resultSummaryAxisY())))
        
        # format layout
        p1 <- p1 + theme_light(base_size = 16)
        if (input$darkTheme) {p1 <- p1 + theme_darker(base_size = 16)}
        
        # If selected, rotate plot 90 degrees C
        if (input$rotatePlot == TRUE) {
          p1 <- p1 + coord_flip(ylim = c(rng[1], rng[2]))
        }
        
        # if title specified
        if (input$addTitle)
          p1 <- p1 + ggtitle(input$title)
        
        # if tidy data, use the labels from selected columns
        if (!is.null(input$labelAxes)) {
          if (!is.null(input$tidyInput)) {
            if (!input$labelAxes && input$tidyInput == TRUE) {
              xlabTitle <- paste(input$labX)
              ylabTitle <- paste(input$labY)
            }
          } else if (input$labelAxes) {
            xlabTitle <- input$labX
            ylabTitle <- input$labY
          }
        }
        
        # if font size is adjusted
        if (input$adjFontSize == TRUE) {
          p1 <- p1 + theme(axis.text = element_text(size = input$adjFontSizeAxLabels))
          p1 <- p1 + theme(axis.title = element_text(size = input$adjFontSizeAxTitle))
        }
        
        # remove legend (if selected)
        if (input$addDescription == FALSE) {  
          p1 <- p1 + theme(legend.position = "none")
        }
        
        # remove gridlines (if selected)
        if (input$noGrid == TRUE) {  
          p1 <- p1 + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            )
        }
        
        if (!is.null(input$adjustColors) && input$adjustColors > 1) {
          p1 <- p1 + scale_color_manual(values = newColors)
          p1 <- p1 + scale_fill_manual(values = newColors)
        }
        
        # # add facet wrap for category?
        # p1 <- p1 + facet_grid(.~cat) +
        #   theme(
        #     strip.background = element_rect(fill = NA, color = "black", size = 2),
        #     strip.text.x = element_text(color = "black")
        #   )
        
        # show titles?
        if (input$showFacetTitles == FALSE) {
          p1 <- p1 +
            theme(
              strip.text.x = element_blank()
            )
        }
        
        # add axis titles
        p1 + xlab(xlabTitle) + ylab(ylabTitle) +
          theme(
            # hide legend title
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            # line thickness
            axis.line = element_line(colour = "black", size = 1),
            panel.border = element_blank(),
            axis.ticks = element_line(colour = "black", size = 1)
            )
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
        resultParamsClustering()
        ), {
        # req(moduleManagers()$selectionManager$selectedUIDs())
        req(cciaSet())
        req(selectedUIDs())
        req(resultParamsClustering())
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        popDT(
          as.data.table(cciaEnv()$LabelPropsUtils(
            cciaSet()$persistentObjectDirectory(),
            value_name = resultParamsClustering())$label_props_view()$as_df())
        )
        
        progress$close()
      })
      
      # create summary DT
      observeEvent(c(popDT(), resultSummaryAxisY()), {
        req(popDT())
        req(resultSummaryAxisY())
        
        # make summary
        # TODO data.table only
        summaryDT(as.data.table(
          popDT() %>%
            # left_join(expInfo()) %>%
            group_by(pop, get(resultSummaryAxisY())) %>%
            summarise(n = n()) %>%
            dplyr::rename_with(
              ~ c(paste0(resultSummaryAxisY(), ".cat")),
              all_of(c("get(resultSummaryAxisY())"))
              ) %>%
            mutate(
              freq = n/sum(n),
            )
        ))
      })
      
      ## Generic
      
      ### UI Outputs
      ## Tables
      
      ## Plots
      
      # plot data
      output$plotData <- renderUI({
        # req(popType())
        req(cciaSet())
        
        # get pop type columns
        clusteringFiles <- list.files(
          file.path(cciaSet()$persistentObjectDirectory(),
                    cciaConf()$dirs$tasks$labelProps),
          pattern = ".sc.")
        clusteringFiles <- str_extract(clusteringFiles, "^.*[^.h5ad]")
        clusteringCols <- c("pop", "clusters")
        
        # propCols <- cciaObj()$labelPropsCols
        # need to get from cluster file
        propCols <- c()
        popTypeChoices <- cciaConf()$parameters$popTypes
        
        # get choices for categories
        
        # create ui elements
        tagList(fluidRow(
          column(
            3,
            tags$label("Parameter plots"),
            selectInput(
              session$ns("resultParamsPopType"), "Population Type",
              choices = .reverseNamedList(popTypeChoices),
              # selected = isolate(popType())
              selected = "live"
              # selected = "clust"
            ),
            createSelectInput(
              session$ns("resultParamsClustering"),
              label = "Clustering to get",
              choices = clusteringFiles,
              multiple = FALSE,
              selected = isolate(resultParamsClustering())
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
            # createSelectInput(
            #   session$ns("resultSummaryAxisX"),
            #   label = "X Axis",
            #   choices = popTypeCols,
            #   multiple = TRUE,
            #   selected = isolate(resultSummaryAxisX())
            #   # selected = c(
            #   #   "live.cell.hmm.state.shape", "live.cell.hmm.state.movement")
            # ),
            createSelectInput(
              session$ns("resultSummaryAxisY"),
              label = "Y Axis",
              choices = clusteringCols,
              multiple = FALSE,
              # selected = isolate(resultSummaryAxisY())
              selected = c("clusters")
              # selected = "live.cell.hmm.state.movement"
              # selected = "clust.cell.contact#clust.TRITC+"
              # selected = c(
              #   "live.cell.angle",
              #   "live.cell.angle",
              #   "compactness",
              #   "extent",
              #   "oblate",
              #   "prolate",
              #   "solidity",
              #   "sphericity",
              #   "surface_area",
              #   "volume"
              #   )
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
