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
      column(3, uiOutput(ns("qcPlots"))),
      column(3, uiOutput(ns("qcTracks"))),
      column(6, uiOutput(ns("tracksPlots"))),
      column(3, uiOutput(ns("tracksTable")))
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
      popGraph <- reactiveVal()
      popTracks <- reactiveVal()
      popTracksPairs <- reactiveVal()
      popTracksInfo <- reactiveVal()
      
      # selected ccia object
      cciaObj <- reactive({
        # moduleManagers()$imageViewerManager$shownImage()
        initCciaObject(pID = pID, uID = selectedUIDs()[[1]], versionID = versionID)()
      })
      
      pixelRes <- reactive({
        req(cciaObj())
        
        cciaObj()$omeXMLPixelRes(invalidate = FALSE)$x
      })
      
      # selected ccia set
      cciaSet <- reactive({
        # moduleManagers()$imageSetManager$selectedSet()
        initCciaObject(pID = pID, uID = uID, versionID = versionID)()
      })
      
      # DEBUG
      selectedUIDs <- reactive({
        # req(expInfo())
        # expInfo()$uID
        
        c("lWinrY")
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
      
      # populations to get
      resultParamsPops <- reactive({
        input$resultParamsPops
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      ## Generic
      
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
          # pops = popsTracked()
          pops = resultParamsPops()
        )
        
        popDT(DT)
        
        progress$set(message = "Get tracks data", value = 80)
        
        tracks <- cciaObj()$tracks(resultParamsPops())
        popTracks(tracks)
        
        # TODO not sure whether I need other information here
        popTracksInfo(cciaObj()$tracksInfo(
          c("live.cell.speed", "live.cell.angle"), parentPop = resultParamsPops()))
        popTracksPairs(as.data.table(celltrackR::analyzeCellPairs(tracks)))
        
        progress$close()
      })
      
      # observe changes to thresholds
      observeEvent(c(
        input$qcTracksInfoSpeedThresh,
        input$qcTracksInfoAngleThresh
      ), {
        browser()
        
        req(popTracksInfo())
        
        speed.thresh <- input$qcTracksInfoSpeedThresh
        angle.thresh <- input$qcTracksInfoAngleThresh
        # popDT[, id := ""]
        # popDT[live.cell.speed.dev >= speed.thresh | live.cell.angle.dev >= angle.thresh, id := track_id]
        tracks.info <- popTracksInfo()
        tracks.info[, id := ""]
        tracks.info[live.cell.speed.sd >= speed.thresh | live.cell.angle.sd >= angle.thresh, id := track_id]
        
        # TODO this should be more elegant
        # it's just that in place operations are not being picked up by shiny loop
        popTracksInfo(NULL)
        popTracksInfo(tracks.info)
      })
      
      observeEvent(c(
        input$qcTrackPairsAngleThresh,
        input$qcTrackPairsDistThresh
      ), {
        req(popTracksPairs())
        
        # these thresholds should be dynamic and set by the user to delete tracks
        # not sure you need a sort of gating here
        # label cellpairs that have both angle and distance below threshold
        angle.thresh <- input$qcTrackPairsAngleThresh
        dist.thresh <- input$qcTrackPairsDistThresh
        
        track.pairs <- popTracksPairs()
        track.pairs[, id := paste0(cell1, "-", cell2)]
        track.pairs[!(angle < angle.thresh & dist < dist.thresh), id := ""]
        
        # TODO this should be more elegant
        # it's just that in place operations are not being picked up by shiny loop
        popTracksPairs(track.pairs)
      })
      
      ### UI Outputs
      output$plotData <- renderUI({
        # req(popType())
        req(cciaObj())
        
        # get pop type columns
        popTypePops <- list()
        
        print(">> LOOP 0")
        
        if (!is.null(popType())) {
          popTypePops <- unname(cciaSet()$popPaths(
            uIDs = selectedUIDs(), popType = popType(), includeFiltered = TRUE))
          popTypeCols <- cciaObj()$labelPropsCols(popType = popType())
        }
        
        # create ui elements
        tagList(fluidRow(
          column(
            3,
            tags$label("Parameter plots"),
            createSelectInput(
              session$ns("resultParamsPops"),
              label = "Populations to get",
              choices = popTypePops,
              # multiple = TRUE,
              multiple = FALSE,
              # selected = isolate(resultParamsPops())
              selected = c("OTI/tracked")
              # selected = c("gBT+", "gBT+/clustered")
            )
          ),
          column(
            3,
            tags$label("Summary plots")
          )
        ))
      })
      
      ## Tables
      
      ## Plots
      output$qcTracksInfo <- renderPlot({
        browser()
        
        req(popTracksInfo())
        req("id" %in% colnames(popTracksInfo()))
        
        ggplot(popTracksInfo(), aes(live.cell.speed.sd, live.cell.angle.sd)) +
          theme_classic() +
          geom_point() +
          geom_text(aes(label = id), color = "red", hjust = -0.1) +
          geom_vline(xintercept = input$qcTracksInfoSpeedThresh, col = "blue", lty = 2) +
          geom_hline(yintercept = input$qcTracksInfoAngleThresh, col = "blue", lty = 2)
      })
      
      output$qcTrackPairs <- renderPlot({
        req(popTracksPairs())
        req("id" %in% colnames(popTracksPairs()))
        
        # Plot; zoom in on the region with small angles and distances
        ggplot(popTracksPairs(), aes(x = dist, y = angle)) +
          geom_point(color = "gray40") +
          geom_text(aes( label = id), color = "red", hjust = -0.1) +
          labs(
            x = "distance between cell pairs",
            y = "angle between cell pairs") +
          coord_cartesian( xlim=c(0,400), ylim=c(0,180) ) +
          geom_hline(yintercept = input$qcTrackPairsAngleThresh, col = "blue", lty=2) +
          geom_vline(xintercept = input$qcTrackPairsDistThresh, col = "blue", lty=2) +
          theme_classic()
      })
      
      output$qcTracksPairs <- renderPlot({
        req(popTracks())
        req(popTracksPairs())
        req("id" %in% colnames(popTracksPairs()))
        
        track.pairs <- popTracksPairs()[!is.na(dist),]$id
        track.pairs <- unique(track.pairs[track.pairs != ""])
        track.pairs <- stringr::str_split(track.pairs, "-")
        
        par.m <- 4
        par.n <- ceiling(length(track.pairs)/par.m)
        
        par(mfrow=c(par.n, par.m))
        
        for (x in track.pairs) {
          plot(popTracks()[c(x[[1]], x[[2]])], main = paste(x, collapse = "-"))
        }
        
        # to highlight in napari
        # paste(unique(unlist(track.pairs)), collapse = ",")
      })
      
      output$qcTracksSD <- renderPlot({
        req(popTracks())
        req(popTracksInfo())
        req("id" %in% colnames(popTracksInfo()))

        track.pairs <- popTracksInfo()[id != "",]$id
        track.pairs <- unlist(table(track.pairs))
        track.pairs <- sort(track.pairs, decreasing = TRUE)

        xRes <- pixelRes()
        
        par.m <- 6
        par.n <- ceiling(length(track.pairs)/par.m)

        par(mfrow=c(par.n, par.m))

        for (i in names(track.pairs)) {
          plot(popTracks()[i], main = paste(i, track.pairs[[i]], sep = "-"))
        }
      })
      
      # QC plots to select thresholds
      output$qcPlots <- renderUI({
        tagList(
          fluidRow(plotOutput(ns("qcTrackPairs"), height = "400px")),
          fluidRow(
            column(6, sliderInput(ns("qcTrackPairsAngleThresh"), "Angle", 0, 180, 90)),
            column(6, sliderInput(ns("qcTrackPairsDistThresh"), "Distance", 0, 100, 10))
          ),
          fluidRow(plotOutput(ns("qcTracksInfo"), height = "400px")),
          fluidRow(
            column(6, sliderInput(ns("qcTracksInfoSpeedThresh"), "Speed", 0, 20, 9, step = 0.5)),
            column(6, sliderInput(ns("qcTracksInfoAngleThresh"), "Angle", 0, 2, 1, step = 0.1)),
          )
        )
      })
      
      # QC resulting tracks as an overview
      output$qcTracks <- renderUI({
        tagList(
          fluidRow(plotOutput(ns("qcTracksPairs"), height = "400px")),
          fluidRow(plotOutput(ns("qcTracksSD"), height = "400px"))
        )
      })
      
      ## Other
    }
  )
}

shinyApp(ui = ui, server = server)
