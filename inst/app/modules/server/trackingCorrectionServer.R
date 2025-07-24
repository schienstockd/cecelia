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
      resultFlushCache <- reactiveVal()
      
      # reactives for storing point/track information
      # this information will come from napari or other
      storeLabels <- reactiveVal()
      storePointsDT <- reactiveVal()
      storePoints <- reactiveVal()
      storeTracks <- reactiveVal()
      tracksModifies <- reactiveVal()
      
      # helpers for modification
      updatePopDT <- reactiveVal(1)
      tracksEditHistory <- reactiveVal()
      tracksCurrentIDs <- reactiveVal()
      
      ### Reactive-like values
      ## dependent on rootDT
      popDT <- eventReactive(c(
        rootDT(),
        updatePopDT()
      ), {
        req(rootDT())
        
        rootDT()[track_id > 0]
      })
      
      # get value name
      valueName <- reactive({
        # TODO this is a bit cheating
        .flowPopParent(resultParamsPops(), getRoot = FALSE)
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
      
      # populations
      popsTracked <- reactive({
        popsList <- cciaObj()$popPaths(popType = popType(), includeFiltered = TRUE)
        
        popsList[!is.na(stringr::str_match(popsList, "/tracked$"))]
      })
      
      # selected ccia object
      cciaObj <- reactive({
        moduleManagers()$imageViewerManager$shownImage()
      })
      
      pixelRes <- reactive({
        req(cciaObj())
        
        cciaObj()$omeXMLPixelRes(invalidate = FALSE)
      })
      
      # generate dataframe from selected image list
      imageData <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        moduleManagers()$imageSetManager$selectedSet()$summary(
          c("Attr"), withSelf = FALSE,
          uIDs = moduleManagers()$imageSetManager$filteredUIDs())
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
          # completeDT = TRUE,
          # replaceNA = TRUE,
          # pops = popsTracked()
          # pops = resultParamsPops()
          # TODO this will get the whole DT for track editing
          pops = c(.flowPopParent(resultParamsPops()), resultParamsPops())
        )
        
        # make sure that the DT order matches the labels order
        # this can happen when you manually add segmentation?
        labels <- cciaObj()$labelProps(valueName = valueName())
        DT[, label := factor(label, levels = labels$values_obs()$label)]
        setorder(DT, label)
        labels$close()
        
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
      
      # listen to viewer output
      observeEvent(globalManagers$viewerManager()$viewerOutput(), {
        req(moduleManagers()$imageViewerManager$shownImage())
        
        viewerOutput <- globalManagers$viewerManager()$viewerOutput()
        outputProcessed <- FALSE
        
        # check whether there is something to do
        if ("trackingCorrectionSelectPoints" %in% names(viewerOutput)) {
          if (length(viewerOutput$trackingCorrectionSelectPoints)) {
            # set selected labels
            req(length(viewerOutput$trackingCorrectionSelectPoints) > 0)
            
            # save points
            storeLabels(viewerOutput$trackingCorrectionSelectPoints)
            
            outputProcessed <- TRUE
          }
        }
        
        # tell the viewer that the command was processed
        if (outputProcessed == TRUE){
          globalManagers$viewerManager()$clearViewerOutput()
        }
      })
      
      # dependent on rootDT
      tracksDT <- eventReactive(rootDT(), {
        req(rootDT())
        
        rootDT()[track_id > 0]
      })
      
      # points depends on selected labels
      observeEvent(c(storeLabels(), rootDT(), updatePopDT()), {
        req(length(storeLabels()) > 0)
        
        # get points DT
        # DT <- copy(rootDT()[label %in% storeLabels()])
        track.ids <- rootDT()[label %in% storeLabels()]$track_id
        DT <- copy(rootDT()[track_id %in% track.ids])
        
        storePointsDT(DT)
        
        # storeTracks(DT)
        # as.data.frame otherwise DT will be unhappy
        storePoints(as.data.frame(DT[
          label %in% storeLabels(), c("track_id", "label", "centroid_t")] %>% rename(uID = label)))
      })
      
      # tracks depends on selected points
      observeEvent(storePoints(), {
        req(nrow(storePoints()) > 0)
        
        storeTracks(unique(storePoints()[, c("track_id"), drop = FALSE]) %>% rename(uID = track_id))
      })
      
      # listen to track modification
      observeEvent(c(
        updatePopDT()
      ), {
        req(updatePopDT())
        req(rootDT())
        req(tracksDT())
        
        # save tracks
        tracks.save.mod(cciaObj(), rootDT(), valueName = valueName())
        
        # call napari
        globalManagers$viewerManager()$viewer()$highlightTracks(
          paste0(valueName(), "-mod"), tracksDT()$track_id, "selected")
        globalManagers$viewerManager()$viewer()$showLabelsAll(
          list(paste0(valueName(), "-mod")), showTracks = TRUE)
      })
      
      # Listen to points/tracks OPs
      observeEvent(input$pointsOpRm, {
        req(length(pointsSelection$selectedUIDs()) > 0)
        
        # get points for operation
        tracks.points.rm(rootDT(), pointsSelection$selectedUIDs())
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("PT.rm", paste(pointsSelection$selectedUIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$pointsOpAdd, {
        req(length(pointsSelection$selectedUIDs()) > 0)
        
        # get points for operation
        tracks.points.add(rootDT(), pointsSelection$selectedUIDs(),
                          trackID = tracksSelection$selectedUIDs())
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("PT.add", paste(pointsSelection$selectedUIDs(), collapse = ","),
                        "to", paste(tracksSelection$selectedUIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$pointsOpSave, {
        req(cciaObj())
        req(valueName())
        
        # save tracking correction
        # copy modified version over to original tracking
        labelsPath <- cciaObj()$imLabelPropsFilepath(valueName())
        modPath <- paste0(
          tools::file_path_sans_ext(labelsPath), "-mod.", tools::file_ext(labelsPath))
        
        file.copy(modPath, labelsPath, overwrite = TRUE)
      })
      
      observeEvent(input$tracksOpRm, {
        req(length(tracksSelection$selectedUIDs()) > 0)
        
        # get points for operation
        tracks.rm(rootDT(), tracksSelection$selectedUIDs())
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("TK.rm", paste(tracksSelection$selectedUIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$tracksOpJoin, {
        req(length(tracksSelection$selectedUIDs()) > 0)
        
        # get points for operation
        tracks.join(rootDT(), tracksSelection$selectedUIDs()[[1]], tracksSelection$selectedUIDs()[[2]])
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("TK.join", paste(tracksSelection$selectedUIDs(), collapse = ",")))
        
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
      
      # points data
      output$pointsTable <- DT::renderDataTable({
        req(storePoints())
        req(nrow(storePoints()) > 0)
        
        # add scollbars to table
        # https://stackoverflow.com/a/73221455
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          pointsSelection$createSelectionColumn(),
          storePoints()
          # moduleManagers()$taskManager$createTaskDataTableColumns()
        ), pageLength = 6, dom = "tip")
      })
      
      # tracks data
      output$tracksTable <- DT::renderDataTable({
        req(storeTracks())
        req(nrow(storeTracks()) > 0)
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          tracksSelection$createSelectionColumn(),
          storeTracks()
          # moduleManagers()$taskManager$createTaskDataTableColumns()
        ), pageLength = 6, dom = "tip")
      })
      
      # Edit history
      output$editHistory <- DT::renderDataTable({
        req(tracksEditHistoryDF())
        
        # get table
        options = list(
          fixedColumns = list(leftColumns = 1),
          fixedHeader = TRUE
        )
        
        tableOpts <- list(
          ordering = TRUE,
          dom = "tip",
          pageLength = 8
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
      
      ## Plots
      output$trackTraces <- renderPlotly({
        req(storePointsDT())
        
        plot_ly(source = "trackTraces") %>%
          add_trace(
            data = storePointsDT()[track_id > 0],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE) %>%
          layout(
            xaxis = list(scaleanchor = "y", scaleratio = 1, range = c(0, max(rootDT()$centroid_x))),
            yaxis = list(autorange = "reversed", range = c(0, max(rootDT()$centroid_y)))
          ) %>%
          toWebGL()
      })
      
      output$pointsPreview <- renderPlotly({
        # req(pointsSelection$selectedUIDs())
        
        plot_ly(source = "pointsPreview") %>%
          add_trace(
            data = storePointsDT()[track_id > 0 & track_id],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE,
            marker = list(color = "lightgrey"), line = list(color = "lightgrey")) %>%
          add_trace(
            data = storePointsDT()[track_id > 0 & label %in% pointsSelection$selectedUIDs()],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "markers", showlegend = FALSE) %>%
          add_trace(
            data = storePointsDT()[is.na(track_id) & label %in% pointsSelection$selectedUIDs()],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "markers", showlegend = FALSE,
            marker = list(color = "#ff1493")) %>%
          layout(
            xaxis = list(scaleanchor = "y", scaleratio = 1, range = c(0, max(rootDT()$centroid_x))),
            yaxis = list(autorange = "reversed", range = c(0, max(rootDT()$centroid_y)))
          ) %>%
          toWebGL()
      })
      
      output$tracksPreview <- renderPlotly({
        # req(tracksSelection$selectedUIDs())
        
        plot_ly(source = "tracksPreview") %>%
          add_trace(
            data = storePointsDT()[track_id > 0 & track_id],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE,
            marker = list(color = "lightgrey"), line = list(color = "lightgrey")) %>%
          add_trace(
            data = storePointsDT()[track_id %in% tracksSelection$selectedUIDs()],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE) %>%
          layout(
            xaxis = list(scaleanchor = "y", scaleratio = 1, range = c(0, max(rootDT()$centroid_x))),
            yaxis = list(autorange = "reversed", range = c(0, max(rootDT()$centroid_y)))
          ) %>%
          toWebGL()
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
      
      output$pointsOps <- renderUI({
        fluidRow(
          actionButton(session$ns("pointsOpRm"), "Remove"),
          actionButton(session$ns("pointsOpAdd"), "Add"),
          actionButton(session$ns("pointsOpSave"), "save")
        )
      })
      
      output$tracksOps <- renderUI({
        fluidRow(
          actionButton(session$ns("tracksOpRm"), "Remove"),
          actionButton(session$ns("tracksOpJoin"), "Join")
        )
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
      
      # selection managers
      pointsSelection <- createSelectionManager(
        input, output, session, globalManagers, moduleManagers,
        list(selectionData = storePoints, selectionID = "points")
      )
      
      tracksSelection <- createSelectionManager(
        input, output, session, globalManagers, moduleManagers,
        list(selectionData = storeTracks, selectionID = "tracks")
      )
    }
  )
}
