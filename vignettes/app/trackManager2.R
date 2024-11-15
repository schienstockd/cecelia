library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

library(flowCore)
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
devtools::load_all("../../")
cciaUse("~/cecelia/dev")
# cciaUse("~/cecelia/dev", initJupyter = TRUE)
# cciaUse("~/cecelia/dev", initConda = FALSE)

# source all files from subdirectories
# TODO is there a better way of doing this?
appSources <- c(
  file.path("..", "inst", "app", "constantsCore.R"),
  list.files(file.path("..", "inst", "app", "lib"), pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("..", "inst", "app", "helpers"), pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("..", "inst", "app", "modules", "managers"), pattern = ".R$", recursive = TRUE, full.names = TRUE)
)

for (x in appSources) {
  source(x)
}

track.border <- list(
  title = "",
  zeroline = FALSE,
  showline = TRUE,
  showticklabels = FALSE,
  showgrid = FALSE
)

# 2P tutorial
pID <- "Co3HDh"
versionID <- 1
uID <- "QnkxJE"

# start ipython kernel
# viewer <- NapariUtils$new()
# viewer$initNapari()

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
      uiOutput(ns("plotData")),
      verbatimTextOutput(ns("plotEvents")),
      verbatimTextOutput(ns("DTEvents"))
    )
  ),
  fluidRow(
    shinydashboard::box(
      id = ns("plots"),
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "QC plots",
      status = "primary",
      width = 12,
      uiOutput(ns("qcPlots"))
    )
  ),
  fluidRow(
    shinydashboard::box(
      id = ns("plots"),
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Track modification",
      status = "primary",
      width = 12,
      column(4, uiOutput(ns("qcTracksOverview"))),
      column(4, uiOutput(ns("qcTracksSelection"))),
      column(4, uiOutput(ns("qcTracksSelectionTable")))
    )
  ),
)

server <- function(input, output, session) {
  moduleServer(
    id,
    function(input, output, session) {
      ### DEBUG
      uiManager <- UIManager$new(input, session)
      
      ### Functions
      createTracksBox <- function(box.id, track.id) {
        plotlyOutput(session$ns(paste0(box.id, "-", track.id)), height = "100px")
      }
      
      ### Reactive values
      # popDT <- reactiveVal()
      rootDT <- reactiveVal()
      updatePopDT <- reactiveVal()
      selectedTracks <- reactiveVal()
      
      ## dependent on popDT
      popDT <- eventReactive(c(
        rootDT(),
        updatePopDT()
        ), {
        req(rootDT())
        
        rootDT()[track_id > 0]
      })
      
      # graph
      popGraph <- eventReactive(popDT(), {
        req(popDT())
        
        cciaObj()$tracksGraph(
          resultParamsPops(),
          completeDT = TRUE,
          replaceNA = TRUE,
          extraAttr = c("label"),
          popDT = popDT()
        )
      })
      
      # tracks
      popTracks <- eventReactive(popDT(), {
        req(popDT())
        req(resultParamsPops())
        
        cciaObj()$tracks(resultParamsPops(), popDT = popDT())
      })
      
      # track pairs
      popTracksPairs <- reactive({
        req(popTracks())
        
        as.data.table(celltrackR::analyzeCellPairs(popTracks()))
      })
      
      # tracks info
      popTracksInfo <- eventReactive(popDT(), {
        req(popDT())
        
        cciaObj()$tracksInfo(
          c("live.cell.speed", "live.cell.angle"),
          parentPop = resultParamsPops(),
          popDT = popDT()
        )
      })
      
      # track pairs selection
      popTracksPairsSelection <- reactive({
        req(popTracksPairs())
        req(tracksPairsSelection())
        
        popTracksPairs()[interaction(cell1, cell2) %in% tracksPairsSelection()]
      })
      
      # tracks info selection
      popTracksInfoSelection <- reactive({
        req(popTracksInfo())
        req(tracksSelection())
        
        popTracksInfo()[track_id %in% tracksSelection()]
      })
      
      # get population IDs from filtered tracks
      popTracksFiltered <- reactive({
        req(popDT())
        
        popDT()[track_id %in% c(
          popTracksInfoSelection()$track_id,
          unique(popTracksPairsSelection()$cell1),
          unique(popTracksPairsSelection()$cell2)
        )]
      })
      
      # get population IDs from selected tracks
      popTracksSelection <- reactive({
        req(popDT())
        req(selectedTrackTraces())
        
        popDT()[track_id %in% selectedTrackTraces()]
      })
      
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
      
      # check tracks from selection
      selectedTrackTraces <- eventReactive(c(
        event_data("plotly_click", "trackTraces"),
        event_data("plotly_selected", "trackTraces")
        ), {
        req(popTracksFiltered())
        trackSelection <- c(
          event_data("plotly_click", "trackTraces"),
          event_data("plotly_selected", "trackTraces")
        )
        req(trackSelection)
        
        # TODO should this be a reactive?
        curveIDs <- unique(trackSelection$curveNumber)
        trackIDs <- unique(popTracksFiltered()$track_id)
        
        # return track IDs
        trackIDs[curveIDs + 1]
      })
      
      # get selected tracks from table
      selectedTrackIDs <- reactive({
        req(selectedTrackTraces())
        
        selectedTrackTraces()[input$qcTracksSelectionDT_rows_selected]
      })
      
      # observe changes to thresholds
      tracksSelection <- eventReactive(c(
        input$qcTracksInfoSpeedThresh,
        input$qcTracksInfoAngleThresh
      ), {
        req(popTracksInfo())
        
        popTracksInfo()[
          live.cell.speed.sd >= input$qcTracksInfoSpeedThresh |
            live.cell.angle.sd >= input$qcTracksInfoAngleThresh, track_id]
      })
      
      tracksPairsSelection <- eventReactive(c(
        input$qcTrackPairsAngleThresh,
        input$qcTrackPairsDistThresh
      ), {
        req(popTracksPairs())
        
        popTracksPairs()[
          angle < input$qcTrackPairsAngleThresh &
            dist < input$qcTrackPairsDistThresh, interaction(cell1, cell2)]
      })
      
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
        req(cciaObj())
        
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
          # pops = resultParamsPops()
          # TODO this will get the whole DT for track editing
          pops = c(.flowPopParent(resultParamsPops()), resultParamsPops())
        )
        
        # set order
        setorder(DT, cell_id)
        
        # popDT(DT)
        rootDT(DT)
        
        progress$close()
        
        ### DEBUG
        # show image
        # viewer$setTaskDir(
        #   cciaObj()$persistentObjectDirectory()
        # )
        # 
        # viewer$openImage(
        #   cciaObj()$imFilepath(valueName = "corrected"),
        #   imChannelNames = cciaObj()$imChannelNames(),
        #   show3D = TRUE, layersVisible = c(F,F,F,F)
        # )
      })
      
      # listen to track selection
      observeEvent(c(
        popTracksFiltered()
      ), {
        req(cciaObj())
        req(popTracksFiltered())
        
        # get value name
        # TODO maybe that should be a reactive
        valueName <- .flowPopParent(resultParamsPops())
        
        # save tracks
        # tracks.save.mod(cciaObj(), popTracksFiltered(), "OTI")
        tracks.save.mod.tracks(cciaObj(), popTracksFiltered()$track_id, "OTI")
        
        # call napari
        # viewer$highlightTracks(paste0(valueName, "-mod"), popTracksFiltered()$track_id, "filtered")
        # viewer$showLabelsAll(list(paste0(valueName, "-mod")), showTracks = TRUE)
      })
      
      # track edit operations
      observeEvent(input$edtTracksJoin, {
        # two tracks are required for join
        req(length(selectedTrackIDs()) == 2)
        
        print(paste(">> JOIN", paste(selectedTrackIDs(), collapse = ",")))
        tracks.join(rootDT(), selectedTrackIDs()[[1]], selectedTrackIDs()[[2]])
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$edtTracksDel, {
        # two tracks are required for join
        req(length(selectedTrackIDs()) > 0)
        
        print(paste(">> DEL", paste(selectedTrackIDs(), collapse = ",")))
        tracks.rm(rootDT(), selectedTrackIDs())
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      ### UI Outputs
      output$plotData <- renderUI({
        # req(popType())
        req(cciaObj())
        
        # get pop type columns
        popTypePops <- list()
        
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
      
      # check events data
      output$plotEvents <- renderText({
        # suppressWarnings(
        #   paste(
        #     # paste("plotly_hover", paste(event_data("plotly_hover", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_unhover", paste(event_data("plotly_unhover", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_click", paste(event_data("plotly_click", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_doubleclick", paste(event_data("plotly_doubleclick", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_selected", paste(event_data("plotly_selected", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_selecting", paste(event_data("plotly_selecting", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_brushed", paste(event_data("plotly_brushed", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_brushing", paste(event_data("plotly_brushing", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_deselect", paste(event_data("plotly_deselect", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_relayout", paste(event_data("plotly_relayout", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_restyle", paste(event_data("plotly_restyle", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_legendclick", paste(event_data("plotly_legendclick", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_legenddoubleclick", paste(event_data("plotly_legenddoubleclick", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_clickannotation", paste(event_data("plotly_clickannotation", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_afterplot", paste(event_data("plotly_afterplot", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_sunburstclick", paste(event_data("plotly_sunburstclick", "trackTraces"), collapse = "; ")),
        #     # paste("plotly_animated", paste(event_data("plotly_animated", "trackTraces"), collapse = "; ")),
        #     # sep = "\r\n"
        #   )
        # )
      })
      
      output$DTEvents = renderPrint({
        # cat('Rows on the current page:\n\n')
        # cat(input$qcTracksSelectionDT_rows_current, sep = ', ')
        # cat('\n\nAll rows:\n\n')
        # cat(input$qcTracksSelectionDT_rows_all, sep = ', ')
        # cat('\n\nSelected rows:\n\n')
        # cat(input$qcTracksSelectionDT_rows_selected, sep = ', ')
      })
      
      ## Tables
      output$qcTracksSelectionDT <- DT::renderDataTable({
        req(popTracksInfo())
        req(selectedTrackTraces())
        
        # get table
        options = list(
          fixedColumns = list(leftColumns = 1),
          fixedHeader = TRUE)
        
        tableOpts <- list(
          ordering = FALSE,
          dom = "tip"
          # pageLength = 10
        )
        
        uiManager$dataTable(
          # popTracksSelection(), options = options, rownames = TRUE, editable = TRUE,
          popTracksInfo()[track_id %in% selectedTrackTraces()], options = options, rownames = TRUE, editable = FALSE,
          # ordering = tableOpts$ordering, pageLength = tableOpts$pageLength, dom = tableOpts$dom)
          ordering = tableOpts$ordering, dom = tableOpts$dom, selection = "multiple")
      }, server = TRUE)
      
      # # update table without triggering redraw
      # observeEvent(c(
      #   popTable(), updatePopTable()
      # ), {
      #   req(popTable())
      #   
      #   # https://stackoverflow.com/a/56879871/13766165
      #   replaceData(dataTableProxy("popTable"),
      #               popTable(),
      #               resetPaging = FALSE, rownames = FALSE)
      # })
      
      ## Plots
      output$qcTracksInfo <- renderPlot({
        req(popTracksInfo())
        
        ggplot(popTracksInfo(), aes(live.cell.speed.sd, live.cell.angle.sd)) +
          theme_classic() +
          geom_point() +
          geom_text(data = popTracksInfoSelection(),
                    aes(label = track_id), color = "red", hjust = -0.1) +
          geom_vline(xintercept = input$qcTracksInfoSpeedThresh, col = "blue", lty = 2) +
          geom_hline(yintercept = input$qcTracksInfoAngleThresh, col = "blue", lty = 2)
      })
      
      output$qcTrackPairs <- renderPlot({
        req(popTracksPairs())
        
        # Plot; zoom in on the region with small angles and distances
        ggplot(popTracksPairs(), aes(x = dist, y = angle)) +
          geom_point(color = "gray40") +
          geom_text(data = popTracksPairsSelection(),
                    aes(label = interaction(cell1, cell2)), color = "red", hjust = -0.1) +
          labs(
            x = "distance between cell pairs",
            y = "angle between cell pairs") +
          # coord_cartesian(xlim = c(0,400), ylim = c(0,180)) +
          geom_hline(yintercept = input$qcTrackPairsAngleThresh, col = "blue", lty=2) +
          geom_vline(xintercept = input$qcTrackPairsDistThresh, col = "blue", lty=2) +
          theme_classic()
      })
      
      output$qcTracksPlot <- renderPlotly({
        req(popTracks())
        req(popTracksFiltered())
        
        plot_ly(source = "trackTraces") %>%
          add_trace(
            data = popTracksFiltered(),
            # data = popDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE) %>%
          layout(
            xaxis = list(scaleanchor = "y", scaleratio = 1),
            yaxis = list(autorange = "reversed"),
            xlim = c(0, max(popDT()$centroid_x)),
            ylim = c(0, max(popDT()$centroid_y))
            ) %>%
          toWebGL()
      })
      
      output$qcTracksSelectedPlot <- renderPlotly({
        req(popTracks())
        req(popTracksSelection())
        
        plot_ly(source = "selectedTrackTraces") %>%
          add_trace(
            data = popTracksSelection(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE) %>%
          layout(
            xaxis = list(scaleanchor = "y", scaleratio = 1),
            yaxis = list(autorange = "reversed"),
            xlim = c(0, max(popDT()$centroid_x)),
            ylim = c(0, max(popDT()$centroid_y))
            ) %>%
          toWebGL()
      })
      
      # QC plots to select thresholds
      output$qcPlots <- renderUI({
        tagList(
          fluidRow(
            column(
              5,
              fluidRow(plotOutput(ns("qcTrackPairs"), height = "300px", width = "300px")),
              fluidRow(
                column(6, sliderInput(ns("qcTrackPairsAngleThresh"), "Angle", 0, 180, 90)),
                column(6, sliderInput(ns("qcTrackPairsDistThresh"), "Distance", 0, 100, 10))
              )
            ),
            column(
              5,
              fluidRow(plotOutput(ns("qcTracksInfo"), height = "300px", width = "300px")),
              fluidRow(
                column(6, sliderInput(ns("qcTracksInfoSpeedThresh"), "Speed", 0, 20, 9, step = 0.5)),
                column(6, sliderInput(ns("qcTracksInfoAngleThresh"), "Angle", 0, 2, 1, step = 0.1)),
              )
            )
          )
        )
      })
      
      # QC resulting tracks as an overview
      output$qcTracksOverview <- renderUI({
        plotlyOutput(ns("qcTracksPlot"), height = "400px", width = "400px")
      })
      
      # track selection for correction
      output$qcTracksSelection <- renderUI({
        tagList(
          fluidRow(
            plotlyOutput(ns("qcTracksSelectedPlot"), height = "400px", width = "400px")
          ),
          fluidRow(
            column(3, actionButton(session$ns("edtTracksJoin"), "join")),
            column(3, actionButton(session$ns("edtTracksDel"), "delete"))
          )
        )
      })
      
      # track table from selection
      output$qcTracksSelectionTable <- renderUI({
        DT::dataTableOutput(ns("qcTracksSelectionDT"))
      })
      
      ## Other
    }
  )
}

shinyApp(ui = ui, server = server)
