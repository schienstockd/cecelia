#' @description Server for tracking images
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.trackingImagesServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      # update image
      updateImage <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      # experiment info
      expInfo <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        
        # get experimental info for set
        as.data.table(moduleManagers()$imageSetManager$selectedSet()$summary(
          withSelf = FALSE, fields = c("Attr")
        ))
      })
      
      # get tracks
      tracks <- eventReactive(c(
        # updateImage(),
        moduleManagers()$imageViewerManager$imageSelected(),
        moduleManagers()$imageViewerManager$imageUpdated()
      ), {
        req(cciaObj())
        
        message(">> GET TRACKS")
        
        # get tracks
        tracksList <- lapply(
          cciaObj()$valueNames("imLabelPropsFilepath"),
          function(x) {
            cciaObj()$tracks(x, forceReload = TRUE)
          })
        
        tracksList[lengths(tracksList) > 0]
      })
      
      # calculate statistics for all tracks
      # TODO should this be done by the process .. ?
      # How would I save this? Into a separate adata file?
      # get stats
      tracksStats <- eventReactive(tracks(), {
        req(length(tracks()) > 0)
        
        tracks.DTs <- list()
        
        for (measure.x in names(tracksMeasures())) {
          # compare measure on plot
          tracks.DTs[[measure.x]] <- tracks.measure.fun(
            # tracks(), get(measure.x), measure.x, idcol = "cell_type")
            tracks(), eval(parse(text = paste0("celltrackR::", measure.x))),
            result.name = measure.x, idcol = "cell_type")
        }
        
        tracks.DTs
      })
      
      # get tracks for whole set
      tracksSet <- reactiveVal()
      tracksStatsSet <- reactiveVal()
        
      ## Generic
      
      # track measurements to show
      tracksMeasures <- reactive(
        list(
          speed = "Speed (\U003BCm/min)",
          duration = "Duration (min)",
          trackLength = "Length (\U003BCm)",
          meanTurningAngle = "Mean turning angle",
          displacement = "Displacement (\U003BCm)",
          straightness = "Straightness",
          displacementRatio = "Displacement Ratio",
          outreachRatio = "Outreach Ratio"
          # asphericity = "Asphericity (1 = straight)",
          # overallAngle = "Overall angle",
        )
      )
      
      # population management
      popType <- reactive({
        "live"
      })
      
      # population data
      popData <- eventReactive(c(
        moduleManagers()$imageViewerManager$imageSelected(),
        cciaObj()
      ), {
        req(cciaObj())
        
        # get object information
        attrToGet <- c(
          "filterMeasure", "filterValues", "filterFun"
        )
        
        # filter for relevant data
        popMap <- cciaObj()$imPopMap(popType(), includeFiltered = TRUE)
        popInfo <- lapply(popMap, function(x) x[names(x) %in% attrToGet])
        
        # create value columns
        datCols <- list()
        
        for (i in attrToGet) {
          datCols[[i]] <- sapply(popInfo, function(x)
            if (!is.null(x[[i]])) x[[i]] else ""
          )
        }
        
        # bind to dataframe
        do.call(cbind, datCols)
      })
      
      # add pop type to task variables
      taskVarsToAdd <- reactive({
        list(
        )
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
      
      # set tracks for set
      # do not reload tracks that are already loaded
      observeEvent(c(
        moduleManagers()$selectionManager$selectedUIDs(),
        moduleManagers()$imageViewerManager$imageSelected(),
        input$tracksPlotsPanel
      ), {
        req(cciaObj())
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$selectionManager$selectedUIDs()) > 0)
        req(input$tracksPlotsPanel == "set")
        
        uIDs <- moduleManagers()$selectionManager$selectedUIDs()
        
        # do not reload already present tracks
        if (!is.null(tracksSet())) {
          uIDs <- uIDs[!uIDs %in% names(tracksSet()[[1]])]
        }
        
        if (length(uIDs) > 0) {
          progress <- Progress$new()
          
          progress$set(
            message = sprintf("Get tracks for %d images", length(uIDs)),
            value = 50)
          
          # get tracks for populations that are shown
          # at the current image
          # TODO you could go through and the value names
          # for all images
          trackList <- lapply(
            cciaObj()$valueNames("imLabelPropsFilepath"),
            function(x) {
              moduleManagers()$imageSetManager$selectedSet()$tracks(
                x, uIDs = uIDs, forceReload = FALSE)
            })
          
          progress$close()
          
          # combine with previous list
          for (x in names(tracksSet())) {
            trackList[[x]] <- append(tracksSet()[[x]], trackList[[x]])
          }
          
          # push back
          tracksSet(trackList)
        }
      })
      
      # calculate statistics for set
      observeEvent(c(
        tracksSet(),
        moduleManagers()$selectionManager$selectedUIDs(),
        input$tracksPlotsPanel
      ), {
        req(length(tracksSet()) > 0)
        req(input$tracksPlotsPanel == "set")
        
        tracks.DTs <- tracksStatsSet()
        
        progress <- Progress$new()
        
        # get uIDs
        uIDs <- names(tracksSet()[[1]])
        if (!is.null(tracksStatsSet())) {
          uIDs <- uIDs[!uIDs %in% unique(tracksStatsSet()[[1]]$uID)]
        }
        
        if (length(uIDs) > 0) {
          # filter uIDs
          
          tracksToCalc <- tracksSet()
          
          for (x in names(tracksToCalc)) {
            tracksToCalc[[x]] <- tracksToCalc[[x]][uIDs]
            
            # remove NULL
            tracksToCalc[[x]] <- tracksToCalc[[x]][lengths(tracksToCalc[[x]]) > 0]
          }
          
          # get stats for all tracks
          for (measure.x in names(tracksMeasures())) {
            progress$set(
              message = sprintf("Calculate %s for %d images", measure.x, length(uIDs)),
              value = 50)
            
            # compare measure on plot
            tracks.DTs[[measure.x]] <- tracks.combine.dt(lapply(
              tracksToCalc, function(x) tracks.measure.fun(
                x, eval(parse(text = paste0("celltrackR::", measure.x))),
                result.name = measure.x)
              # steps.subtracks = 10)
            ))
            
            # combine with previous DT
            if (!is.null(tracksStatsSet())) {
              tracks.DTs[[measure.x]] <- rbind(
                tracksStatsSet()[[measure.x]],
                tracks.DTs[[measure.x]]
              )
            }
          }
        }
        
        progress$close()
        
        if (!is.null(tracks.DTs)) {
          # push back only selected uIDs
          for (x in names(tracksMeasures())) {
            tracks.DTs[[x]] <- tracks.DTs[[x]][uID %in% moduleManagers()$selectionManager$selectedUIDs()]
          }
        }
        
        tracksStatsSet(tracks.DTs)
      }) 
      
      # show track stats
      observeEvent(tracksMeasures(), {
        req(tracksMeasures())
        
        # show plots
        for (x in names(tracksMeasures())) {
          local({
            local_x <- x
            
            # current image
            output[[sprintf("tracksPlotCurrent_%s", local_x)]] <- renderPlot({
              req(tracksStats()[[local_x]])
              
              p1 <- ggplot(tracksStats()[[local_x]] %>% drop_na(),
                           aes(x = cell_type, y = get(local_x), fill = cell_type)) +
                theme_classic() +
                ylab(tracksMeasures()[[local_x]]) +
                xlab("") 
              
              p1 %>% plotThemeViolin() + 
                theme(legend.position = "none")
            })
          })
        }
      })
      
      # show track stats for selected images
      observeEvent(c(
        tracksMeasures(),
        tracksStatsSet()
      ), {
        req(tracksMeasures())
        req(tracksStatsSet())
        
        # show measure plots
        for (x in names(tracksMeasures())) {
          local({
            local_x <- x
            
            # selected images
            output[[sprintf("tracksPlotSelected_%s", local_x)]] <- renderPlot({
              req(tracksStatsSet()[local_x])
              req(input$tracksPlotsSetLayoutAxisX)
              req(input$tracksPlotsSetLayoutAxisFill)
              req(input$tracksPlotsSetLayoutAxisCellTypes)
              
              p1 <- ggplot(
                expInfo()[
                  tracksStatsSet()[[local_x]][
                    cell_type %in% input$tracksPlotsSetLayoutAxisCellTypes],
                  on = c("uID")],
                aes(x = if (input$tracksPlotsSetLayoutAxisInteraction != "NONE")
                  interaction(
                    get(input$tracksPlotsSetLayoutAxisX),
                    get(input$tracksPlotsSetLayoutAxisInteraction)
                    )
                  else
                    get(input$tracksPlotsSetLayoutAxisX),
                  y = get(local_x),
                  fill = get(input$tracksPlotsSetLayoutAxisFill)
                  )) +
                theme_classic() +
                ylab(tracksMeasures()[[local_x]]) +
                xlab("")
              
              p1 %>% plotThemeViolin()
            })
          })
        }
      })
      
      # listen to viewer output
      observeEvent(globalManagers$viewerManager()$viewerOutput(), {
        req(cciaObj())
        
        viewerOutput <- globalManagers$viewerManager()$viewerOutput()
        outputProcessed <- FALSE
        
        # check whether there is something to do
        if ("trackingImagesSelectPoints" %in% names(viewerOutput)) {
          if (length(viewerOutput$trackingImagesSelectPoints)) {
            # set selected labels
            moduleManagers()$flowPlotManager$viewerSelectedLabels(
              viewerOutput$trackingImagesSelectPoints)
            
            outputProcessed <- TRUE
          }
        }
        
        # tell the viewer that the command was processed
        if (outputProcessed == TRUE) {
          globalManagers$viewerManager()$clearViewerOutput()
        }
      })
      
      # listen to data selection
      # this will also trigger when the same image is chosen again
      # selected - user selects
      # shown - the image shown
      observeEvent(moduleManagers()$imageViewerManager$imageSelected(), {
        req(cciaObj())
        
        # collapse selection box
        js$collapseBox(session$ns("imageTableBox"))
        
        # update image
        updateImage(runif(1))
      })
      
      # update image if object is changed
      observeEvent(cciaObj(), {
        req(cciaObj())
        
        # update image
        updateImage(runif(1))
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
          # moduleManagers()$taskManager$createTaskDataTableColumns() 
        ))
      })
      
      ## Plots
      output$tracksPlotsCurrent <- renderUI({
        req(tracksMeasures())
        
        # create plots
        tracksPlots <- list()
        for (x in names(tracksMeasures())) {
          tracksPlots[[x]] <- column(3, plotOutput(
            session$ns(sprintf("tracksPlotCurrent_%s", x)),
            height = "300px", width = "100%"
          ))
        }
        
        # return layout
        tagList(
          list(tracksPlots, br(), br(),
               downloadButton(session$ns("downloadTracksStatsCSV"), "Download csv-file"))
        )
      })
      
      # show same stats for each image that is selected
      output$tracksPlotsSet <- renderUI({
        req(tracksMeasures())
        req(length(moduleManagers()$selectionManager$selectedUIDs()) > 0)
        
        # create plots
        tracksPlotsLayout <- list()
        
        # add number
        measures <- tracksMeasures()
        measures <- append(
          list(number = "Number of Tracks"),
          measures
        )
        
        # create measures
        for (x in names(measures)) {
          tracksPlot <- plotOutput(
            session$ns(sprintf("tracksPlotSelected_%s", x)),
            height = "300px", width = "100%")
          
          tracksPlotsLayout[[x]] <- tabPanel(
            measures[[x]],
            value = x,
            tags$br(),
            tracksPlot
          )
        }
        
        # return layout
        do.call(
          tabsetPanel,
          c(
            unname(tracksPlotsLayout),
            list(
              type = "tabs",
              selected = "number"
            )
          )
        )
      })
      
      # show layout parameters for uID selection plots
      output$tracksPlotsSetLayout <- renderUI({
        req(cciaObj())
        req(tracksMeasures())
        
        # create selection inputs
        fluidRow(
          column(2, selectInput(
            session$ns("tracksPlotsSetLayoutAxisX"),
            "X Axis",
            choices = colnames(expInfo()),
            selected = shinyInputValue(
              "tracksPlotsSetLayoutAxisX", input, colnames(expInfo())[[1]]
            )
          )),
          column(2, selectInput(
            session$ns("tracksPlotsSetLayoutAxisInteraction"),
            "Interact with",
            choices = c("NONE", colnames(expInfo())),
            selected = shinyInputValue(
              "tracksPlotsSetLayoutAxisInteraction", input, "NONE"
            )
          )),
          column(2, selectInput(
            session$ns("tracksPlotsSetLayoutAxisFill"),
            "Fill value",
            choices = append(
              list("Cell type" = "cell_type"),
              colnames(expInfo())
            ),
            selected = shinyInputValue(
              "tracksPlotsSetLayoutAxisFill", input, "cell_type"
            )
          )),
          column(3, selectInput(
            session$ns("tracksPlotsSetLayoutAxisCellTypes"),
            "Cell types",
            choices = cciaObj()$valueNames("imLabelPropsFilepath"),
            multiple = TRUE,
            selected = shinyInputValue(
              "tracksPlotsSetLayoutAxisCellTypes", input,
              cciaObj()$valueNames("imLabelPropsFilepath")
            )
          )),
          column(2, br(), downloadButton(session$ns("downloadTracksStatsSetCSV"), "Download csv-file"))
        )
      })
      
      # show output for number of tracks
      output$tracksPlotSelected_number <- renderPlot({
        req(tracksSet())
        req(input$tracksPlotsSetLayoutAxisX)
        req(input$tracksPlotsSetLayoutAxisFill)
        req(input$tracksPlotsSetLayoutAxisCellTypes)
        
        # convert number of tracks to DF
        # https://stackoverflow.com/a/43306018/13766165
        # https://stackoverflow.com/a/44981374/13766165
        # https://stackoverflow.com/a/54717250/13766165
        numTracks <- as.data.table(
          enframe(unlist(lapply(tracksSet(), lengths))) %>%
            dplyr::mutate(cell_type = gsub("\\.[^.]*$", "", name)) %>%
            rowwise() %>%
            dplyr::mutate(uID = gsub(sprintf("%s.", cell_type), "", name))
        )
        
        p1 <- ggplot(
          expInfo()[numTracks[
            cell_type %in% input$tracksPlotsSetLayoutAxisCellTypes
          ], on = c("uID")],
          aes(x = if (input$tracksPlotsSetLayoutAxisInteraction != "NONE")
            interaction(
              get(input$tracksPlotsSetLayoutAxisX),
              get(input$tracksPlotsSetLayoutAxisInteraction)
            )
            else
              get(input$tracksPlotsSetLayoutAxisX),
            y = value,
            fill = get(input$tracksPlotsSetLayoutAxisFill)
          )) +
          ylab("Number of tracks") +
          xlab("")
        
        p1 %>% plotThemeBar()
      })
      
      ## Buttons
      # show summary table for experimental info
      output$tracksPlotsSummary <- renderUI({
        tags$label("TODO Overview table with mice and treatments here")
      })
      
      # CSV download
      output$downloadTracksStatsCSV <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".csv", sep = "")
        },
        content <- function(file) {
          data.table::fwrite(data.table::rbindlist(tracksStats(), idcol = "measure"), file)
        },
        contentType = "text/csv" # MIME type of the file
      )
      
      output$downloadTracksStatsSetCSV <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".csv", sep = "")
        },
        content <- function(file) {
          data.table::fwrite(data.table::rbindlist(tracksStatsSet(), idcol = "measure"), file)
        },
        contentType = "text/csv" # MIME type of the file
      )
      
      ## Other
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet",
        "imageViewer", "population", "flowPlot")
      managerConf = list(
        moduleName = id,
        selectionData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "tracking_images"
        ),
        task = list(
          funLabel = "Tracking method",
          taskVarsToAdd = taskVarsToAdd
        ),
        population = list(
          popData = popData,
          popType = popType,
          updateImage = updateImage,
          enableFilterPopulation = TRUE,
          popsFilteredFromValueName = TRUE
        ),
        flowPlot = list(
          popType = popType,
          numFlowPlots = reactive(2),
          flowUseFlowColours = reactive(TRUE),
          flowMarkerOpacity = reactive(1.0),
          showFilters = TRUE
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
