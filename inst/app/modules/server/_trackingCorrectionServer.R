#' @description Server for tracking correction
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.trackingCorrectionServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      # record track edits
      recordTrackEdits <- function(x, ...) {
        # record changes
        tracksEditHistory(
          append(tracksEditHistory(), list(track.diffs(tracksCurrentIDs(), x, ...))))
        
        # set new track ids
        tracksCurrentIDs(x)
      }
      
      ### Reactive values
      # population DT
      rootDT <- reactiveVal()
      updatePopDT <- reactiveVal(1)
      tracksEditHistory <- reactiveVal()
      tracksCurrentIDs <- reactiveVal()
      
      # flush cache?
      resultFlushCache <- reactiveVal()
      
      ### Reactive-like values
      ## dependent on rootDT
      popDT <- eventReactive(c(
        rootDT(),
        updatePopDT()
      ), {
        req(rootDT())
        
        rootDT()[track_id > 0]
      })
      
      # get popDT for selected track traces
      selectedTracksDT <- eventReactive(c(
        rootDT(),
        updatePopDT(),
        selectedTrackTraces()
      ), {
        rootDT()[track_id %in% selectedTrackTraces()]
      })
      
      # get value name
      valueName <- reactive({
        # TODO this is a bit cheating
        .flowPopParent(resultParamsPops(), getRoot = FALSE)
      })
      
      # return tracking history as DF
      tracksEditHistoryDF <- reactive({
        # get data
        DT <- as.data.table(list(Function = sapply(tracksEditHistory(), function(x) x$short)))
        nEdits <- nrow(DT)
        modCols <- list()
        
        # prepare modifier columns
        if (nEdits > 0) {
          editIDs <- seq(nEdits)
          modCols <- list(
            "Rollback" = shinyInput(
              "actionLink", session$ns("trackEditRollback_"), editIDs,
              initIcons = rep(btnICON_POINT_LEFT, nEdits),
              initOnclick = paste(
                sprintf(
                  'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
                  session$ns("trackEditRollback"),
                  seq(editIDs)
                )
              )
            )
          )
        }
        
        # bind
        cbind(DT, do.call(cbind, modCols))
      })
      
      ### Reactives - RxCalc
      ## Event specific
      
      # update image automatically when populations are gated
      updateImage <- eventReactive(c(
        cciaObj()
      ), {
        req(cciaObj())
        
        # update image
        runif(1)
      })
      
      ## Generic
      # populations to show
      resultParamsPops <- reactive({
        input$resultParamsPops
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # experiment info
      expInfo <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        
        # get experimental info for set
        moduleManagers()$imageSetManager$selectedSet()$summary(
          withSelf = FALSE, fields = c("Attr")
        )
      })
      
      # population management
      popType <- reactive({
        "live"
      })
      
      # add pop type to task variables
      taskVarsToAdd <- reactive({
        req(popType())
        
        list(
          popType = popType()
        )
      })
      
      # population data
      popData <- reactive({
        req(cciaObj())
        
        moduleManagers()$populationManager$createPopData()
      })
      
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
      
      # populations
      popsTracked <- reactive({
        popsList <- cciaObj()$popPaths(popType = popType(), includeFiltered = TRUE)
        
        popsList[!is.na(stringr::str_match(popsList, "/tracked$"))]
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
        
        cciaObj()$tracks(resultParamsPops(), popDT = popDT(), convertToPhysical = FALSE)
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
      
      pixelRes <- reactive({
        req(cciaObj())
        
        cciaObj()$omeXMLPixelRes(invalidate = FALSE)
      })
      
      # selected ccia object
      cciaObj <- reactive({
        moduleManagers()$imageViewerManager$shownImage()
      })
      
      # generate dataframe from selected image list
      imageData <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        moduleManagers()$imageSetManager$selectedSet()$summary(
          c("Attr"), withSelf = FALSE,
          uIDs = moduleManagers()$imageSetManager$filteredUIDs())
      })
      
      ### Observers - RxAction
      ## Event specific
      
      # population DT for set
      observeEvent(c(
        moduleManagers()$imageSetManager$selectedSet(),
        # moduleManagers()$selectionManager$selectedUIDs(),
        moduleManagers()$imageViewerManager$imageUpdated(),
        resultParamsPops(),
        expInfo()
      ), {
        req(moduleManagers()$imageSetManager$selectedSet())
        req(expInfo())
        req(resultParamsPops())
        req(pixelRes())
        # req(length(moduleManagers()$selectionManager$selectedUIDs()) > 0)
        
        # get uIDs
        # uIDs <- moduleManagers()$selectionManager$selectedUIDs()
        
        # flush cache?
        flushCache <- TRUE
        
        if (!is.null(resultFlushCache())) {
          # TODO is there a better way to do this?
          # ie/ if other images are selected but the
          # underlying data has not changed
          # do not flush the cache
          if (resultFlushCache() == moduleManagers()$imageViewerManager$imageUpdated()) {
            flushCache <- FALSE
          }
        }
        
        progress <- Progress$new()
        
        progress$set(
          message = sprintf("Get DTs for image (flush cache = %s)", flushCache),
          value = 50)
        
        # popDT(cciaObj()$popDT(
        DT <- cciaObj()$popDT(
          popType = popType(),
          includeFiltered = TRUE,
          completeDT = TRUE,
          replaceNA = TRUE,
          # pops = popsTracked()
          # pops = resultParamsPops()
          # TODO this will get the whole DT for track editing
          pops = c(.flowPopParent(resultParamsPops()), resultParamsPops())
        )
        
        # record track ids
        # copy here otherwise the sequence gets changed whenever DT changes
        tracksCurrentIDs(copy(DT$track_id))
        
        # convert to physical values
        rootDT(convertPixelToPhysical(DT, pixelRes()))
        
        # set cache flush
        resultFlushCache(moduleManagers()$imageViewerManager$imageUpdated())
        
        progress$close()
      })
      
      # listen to data selection
      # this will also trigger when the same image is chosen again
      # selected - user selects
      # shown - the image shown
      observeEvent(moduleManagers()$imageViewerManager$imageSelected(), {
        req(cciaObj())
        
        # collapse selection box
        js$collapseBox(session$ns("imageTableBox"))
        
        # save pop map
        cciaObj()$savePops(
          popType(), includeFiltered = TRUE, purge = TRUE)
      })
      
      # listen to track modification
      observeEvent(c(
        popTracksFiltered()
      ), {
        req(cciaObj())
        req(popTracksFiltered())
        req(updatePopDT())
        
        # save tracks
        # tracks.save.mod.tracks(cciaObj(), popTracksFiltered()$track_id, valueName = valueName())
        tracks.save.mod(cciaObj(), rootDT(), valueName = valueName())
        
        # call napari
        # TODO this is different
        globalManagers$viewerManager()$viewer()$highlightTracks(
          paste0(valueName(), "-mod"), popTracksFiltered()$track_id, "filtered")
        globalManagers$viewerManager()$viewer()$showLabelsAll(
          list(paste0(valueName(), "-mod")), showTracks = TRUE)
      })
      
      # listen to track selection
      observeEvent(c(
        selectedTracksDT()
      ), {
        req(cciaObj())
        req(selectedTracksDT())
        
        # call napari and centre
        # get centre position from tracks
        globalManagers$viewerManager()$viewer()$centre(
          list(
            # TODO this should bias less towards longer tracks
            # selectedTracksDT()[, median(centroid_y), by = track_id][, median(V1)],
            # selectedTracksDT()[, median(centroid_x), by = track_id][, median(V1)]
            selectedTracksDT()[, median(centroid_y)],
            selectedTracksDT()[, median(centroid_x)]
          ),
          # selectedTracksDT()[, median(centroid_t), by = track_id][, median(V1)],
          selectedTracksDT()[, max(centroid_t)],
          # TODO I don't have a good calculation for that
          10
        )
        
        # globalManagers$viewerManager()$viewer()$highlightTracks(
        #   paste0(valueName(), "-mod"), popTracksFiltered()$track_id, "filtered")
      })
      
      # track edit operations
      observeEvent(input$edtTracksJoin, {
        # two tracks are required for join
        req(length(selectedTrackIDs()) == 2)
        
        tracks.join(rootDT(), selectedTrackIDs()[[1]], selectedTrackIDs()[[2]])
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("JOIN", paste(selectedTrackIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$edtTracksDel, {
        # two tracks are required for join
        req(length(selectedTrackIDs()) > 0)
        
        tracks.rm(rootDT(), selectedTrackIDs())
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          paste("DEL", paste(selectedTrackIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$edtTracksSave, {
        # save tracking correction
        # copy modified version over to original tracking
        labelsPath <- cciaObj()$imLabelPropsFilepath(valueName())
        modPath <- paste0(
          tools::file_path_sans_ext(labelsPath), "-mod.", tools::file_ext(labelsPath))
        
        file.copy(modPath, labelsPath, overwrite = TRUE)
      })
      
      # rollback changes
      observeEvent(input$trackEditRollback, {
        req(cciaObj())
        
        # update track IDs
        rollback.result <- track.edits.rollback(
          tracksCurrentIDs(), tracksEditHistory(), as.numeric(input$trackEditRollback))
        
        # update DT and ids
        rootDT()[, track_id := rollback.result$x]
        tracksCurrentIDs(rollback.result$x)
        
        # update history
        tracksEditHistory(rollback.result$edit.history)
        
        updatePopDT(runif(1))
      })
      
      ## Generic
      
      ### UI Outputs
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
        
        moduleManagers()$uiManager$dataTable(
          # popTracksSelection(), options = options, rownames = TRUE, editable = TRUE,
          popTracksInfo()[track_id %in% selectedTrackTraces()], options = options, rownames = TRUE, editable = FALSE,
          # ordering = tableOpts$ordering, pageLength = tableOpts$pageLength, dom = tableOpts$dom)
          ordering = tableOpts$ordering, dom = tableOpts$dom, selection = "multiple")
      }, server = TRUE)
      
      # images
      output$imageTable <- DT::renderDataTable({
        req(imageData())
        req(nrow(imageData()) > 0)
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          moduleManagers()$selectionManager$createSelectionColumn(),
          moduleManagers()$imageViewerManager$createShowImageColumn(),
          imageData()
          # behaviour is calculated on a set level
          # # moduleManagers()$taskManager$createTaskDataTableColumns() 
        ))
      })
      
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
              fluidRow(plotOutput(session$ns("qcTrackPairs"), height = "300px", width = "300px")),
              fluidRow(
                column(6, sliderInput(session$ns("qcTrackPairsAngleThresh"), "Angle", 0, 180, 180)),
                column(6, sliderInput(session$ns("qcTrackPairsDistThresh"), "Distance", 0, 100, 10))
              )
            ),
            column(
              5,
              fluidRow(plotOutput(session$ns("qcTracksInfo"), height = "300px", width = "300px")),
              fluidRow(
                column(6, sliderInput(session$ns("qcTracksInfoSpeedThresh"), "Speed", 0, 20, 9, step = 0.5)),
                column(6, sliderInput(session$ns("qcTracksInfoAngleThresh"), "Angle", 0, 2, 1, step = 0.1)),
              )
            )
          )
        )
      })
      
      # Edit history
      output$editHistory <- DT::renderDataTable({
        req(tracksEditHistoryDF())

        # get table
        options = list(
          fixedColumns = list(leftColumns = 1),
          fixedHeader = TRUE)

        tableOpts <- list(
          ordering = TRUE,
          dom = "tip",
          pageLength = 10
        )
        
        moduleManagers()$uiManager$dataTable(
          tracksEditHistoryDF(), options = options, rownames = TRUE, editable = FALSE,
          ordering = tableOpts$ordering, pageLength = tableOpts$pageLength, dom = tableOpts$dom)
      }, server = TRUE)
      
      # update table without triggering redraw
      observeEvent(c(
        tracksEditHistoryDF()
        # updatePopTable()
      ), {
        req(tracksEditHistoryDF())

        # https://stackoverflow.com/a/56879871/13766165
        replaceData(dataTableProxy("editHistory"),
                    tracksEditHistoryDF(),
                    resetPaging = FALSE, rownames = FALSE)
      })
      
      # QC resulting tracks as an overview
      output$qcTracksOverview <- renderUI({
        plotlyOutput(session$ns("qcTracksPlot"), height = "400px", width = "400px")
      })
      
      # track selection for correction
      output$qcTracksSelection <- renderUI({
        tagList(
          fluidRow(
            plotlyOutput(session$ns("qcTracksSelectedPlot"), height = "400px", width = "400px")
          ),
          fluidRow(
            column(2, actionButton(session$ns("edtTracksJoin"), "join")),
            column(2, actionButton(session$ns("edtTracksDel"), "delete")),
            column(2, actionButton(session$ns("edtTracksSave"), "save"))
          )
        )
      })
      
      # track table from selection
      output$qcTracksSelectionTable <- renderUI({
        DT::dataTableOutput(session$ns("qcTracksSelectionDT"))
      })
      
      ## Buttons
      
      ## Other
      output$resultParams <- renderUI({
        req(cciaObj())
        
        # get live columns
        liveCols <- cciaObj()$labelPropsCols(popType = "live")
        
        # create ui elements
        tagList(fluidRow(
          column(
            3,
            tags$label("Parameter plots"),
            createSelectInput(
              session$ns("resultParamsPops"),
              label = "Populations",
              choices = unname(cciaObj()$popPaths(
                popType(), includeFiltered = TRUE)),
              multiple = FALSE,
              selected = resultParamsPops()
            )
          )
        ))
      })
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet",
        "imageViewer", "population")
      managerConf = list(
        moduleName = id,
        selectionData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "tracking_correction"
        ),
        task = list(
          funLabel = "Tracking correction method",
          taskVarsToAdd = taskVarsToAdd,
          runTaskCombinedSelect = TRUE
        ),
        population = list(
          popData = popData,
          popType = popType,
          updateImage = updateImage,
          enableFilterPopulation = TRUE
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
