#' @description Server to clust populations
#' @importFrom plotly renderPlotly
#' 
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.clustPopulationsServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      # save populations
      clustSavePops <- function(pops = NULL, purge = FALSE) {
        # get all populations
        if (is.null(pops)) {
          pops <- cciaObj()$popPaths(popType(), includeFiltered = TRUE)
        }
        
        # save populations
        cciaObj()$savePops(
          popType(), pops, purge = purge,
          includeFiltered = TRUE, tracksOnly = isTrack())
      }
      
      # get umap plot
      plotUMAP <- function(umapCol, sourceID, plotType = "factor") {
        if (umapCol %in% names(adataDT())) {
          # get unique colours
          colourPalette <- NULL
          if (plotType == "factor") {
            colourPalette <- randomcoloR::distinctColorPalette(
              length(unique(adataDT()[[umapCol]]))
            )
          } else if (plotType == "pops") {
            # get colours from populations
            # use grey for root
            colourPalette <- c("grey", unname(sapply(imPopMap(), function(x) x$colour)))
            names(colourPalette) <- c("root", unname(sapply(imPopMap(), function(x) x$path)))
            
            # order by DT
            colourPalette <- unlist(colourPalette[unique(adataDT()$pop)])
          }
          
          p1 <- plot_ly(
            adataDT(),
            x = ~UMAP_1, y = ~UMAP_2,
            type = "scatter", mode = "markers", source = sourceID,
            # https://stackoverflow.com/questions/39859647/set-marker-size-in-plotly
            # marker = list(sizeref = 0.1, sizemode="area")
            # marker = list(size = 5),
            marker = list(size = 2),
            # size = 0.01,
            color = ~get(umapCol), colors = colourPalette) %>%
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
            ) %>% colorbar(
              title = umapCol
              # limits = datasetUMAPGeneExprLim()
            )
          # config(modeBarButtonsToRemove = plotlyModeBarButtonsToRemove)
          
          p1 %>%
            toWebGL()
        }
      }
      
      # get adata DT
      getAdataDT <- function(flushCache = FALSE) {
        if (!is.null(clusteringPartOf())) {
          # get objects from selected set
          moduleManagers()$imageSetManager$selectedSet()$popDT(
            popType = popType(),
            uIDs = clusteringPartOf(),
            includeFiltered = TRUE,
            completeDT = FALSE, replaceNA = TRUE,
            flushCache = flushCache,
            # only focus on clustered values
            filterMeasures = c(clusterColName()),
            tracksOnly = isTrack(),
            valueName = valueName()
          )
        } else {
          # anndata
          cciaObj()$popDT(popType(), includeFiltered = TRUE,
                          completeDT = FALSE, replaceNA = TRUE,
                          flushCache = flushCache,
                          # only focus on clustered values
                          filterMeasures = c(clusterColName()),
                          tracksOnly = isTrack(),
                          valueName = valueName())
        }
      }
      
      ### Reactive values
      # general
      labelProps <- reactiveVal()
      
      adataDT <- reactiveVal()
      adataMat <- reactiveVal()
      
      # population table
      popTable <- reactiveVal()
      
      # viewer
      viewerSelectedLabels <- reactiveVal()
      viewerUpdatePops <- reactiveVal()
      
      # population management
      popType <- reactive("clust")
      popTypeProcessing <- reactive(input$popType)
      # TODO cannot get my head around this
      valueName <- reactive(input$valueName)
      # valueName <- reactive("default")
      clusterColName <- reactive(input$clusterColName)

      # add pop type to task variables
      taskVarsToAdd <- reactive({
        req(popTypeProcessing())
        
        list(
          popType = popTypeProcessing()
        )
      })
      
      # marker selection for umap
      umapMarkerSelection <- reactive({
        req(input$umapMarkerSelection)
        
        input$umapMarkerSelection
      })
      
      # attribute selection for umap
      umapAttrSelection <- reactive({
        req(input$umapAttrSelection)
        
        input$umapAttrSelection
      })
      
      # set clustering part of
      clusteringPartOf <- reactive({
        req(cciaObj())
        
        # is the clustering a part of a set clustering?
        cciaObj()$valuePartOf("imAnndataFilepath")
      })
      
      setClusterForPop <- reactive({
        req(input$setClusterForPop)
        
        input$setClusterForPop
      }) %>% debounce(cciaConf()$fcs$gating$plots$poll)
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      # population map
      # trigger pop map when image is selected
      # imPopMap <- reactive({
      imPopMap <- eventReactive(c(
        moduleManagers()$imageViewerManager$imageSelected(),
        cciaObj(),
        clusterColName()
        ), {
        req(cciaObj())
        
        cciaObj()$imPopMap(popType(), includeFiltered = TRUE, filterMeasures = c(clusterColName()))
      })
      
      # get value names
      # valueName <- reactive({
      #   req(adataDT())
      #   
      #   if ("value_name" %in% colnames(adataDT()))
      #     unique(adataDT()$value_name)
      #   else
      #     c("default")
      # })
      
      # is tracking data?
      isTrack <- reactive({
        # TODO chicken-egg problem
        # req(adataDT())
        # "track_id" %in% colnames(adataDT())
        globalManagers$projectManager()$getProjectType() == "live"
      })
      
      # population data
      popData <- reactive({
        req(cciaObj())
        req(imPopMap())
        req(adataMat())
        
        # prepare population dataframe
        zeroPops <- matrix(
          0,
          ncol = ncol(adataMat()),
          nrow = length(imPopMap()))
        
        # set names
        colnames(zeroPops) <- colnames(adataMat())
        rownames(zeroPops) <- names(imPopMap())
        
        # order by dendrogram
        popsDF <- as.data.frame(zeroPops)
        popsDF <- popsDF[, adataColDendLabels()]
        popIDs <- rownames(popsDF)
        
        # for every column init a link to change
        clusterCols <- list()
        inputID <- "setClusterForPop"
        
        if (length(imPopMap()) > 0) {
          for (x in colnames(popsDF)) {
            curID <- sprintf("%s_%s_", inputID, x)
            # curValues <- imPopMap()[, x]
            curValues <- unlist(
              lapply(popIDs, function(i) if (x %in% imPopMap()[[i]]$filterValues) 1 else 0)
            )
            
            # set pop icons
            popIcons <- rep(btnICON_NOTSELECTED, length(popIDs))
            popIcons[curValues > 0] <- btnICON_SELECTED
            
            # create column
            curCol <- list(
              " " = shinyInput(
                "actionLink", session$ns(curID), popIDs,
                initIcons = popIcons,
                initOnclick = paste(
                  sprintf(
                    'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
                    session$ns(inputID),
                    paste(popIDs, x, sep = ".")
                  )
                )
              )
            )
            
            # add to list
            clusterCols <- append(
              clusterCols, curCol)
          }
        }
        
        # create dataframe
        popsDFwithCLusters <- do.call(cbind, clusterCols)
        
        if (!is.null(popsDFwithCLusters)) {
          names(popsDFwithCLusters) <- colnames(popsDF)
          colnames(popsDFwithCLusters) <- colnames(popsDF)
          rownames(popsDFwithCLusters) <- rownames(popsDF)
        }
        
        popsDFwithCLusters
      })
      
      # selected ccia object
      cciaObj <- reactive({
        moduleManagers()$imageViewerManager$shownImage()
      })
      
      # update image automatically when populations are gated
      updateImage <- eventReactive(c(
        cciaObj(), viewerUpdatePops()
      ), {
        req(moduleManagers()$populationManager$autoUpdateImage())
        req(cciaObj())
        
        # update image
        runif(1)
      })
      
      # generate dataframe from selected image list
      imageData <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        moduleManagers()$imageSetManager$selectedSet()$summary(
          c("Attr"), withSelf = FALSE,
          uIDs = moduleManagers()$imageSetManager$filteredUIDs())
      })
      
      # dendrograms for clusters
      adataColDendH <- reactive({
        req(adataMat())
        
        # heatmap
        adataMat() %>%
          t %>% dist %>% hclust %>% as.dendrogram %>%
          ladderize
      })
      
      adataColDendC <- reactive({
        req(adataMat())
        
        # find correlation
        corMat <- cor(adataMat())
        corMat[is.na(corMat)] <- 0
        
        # create dendrogram if only two clusters were found
        if (ncol(corMat) == 2) {
          # https://stackoverflow.com/a/18895310
          tree <- list()
          attributes(tree) <- list(members = 2, height = 1)
          class(tree) <- "dendrogram"
          
          tree[[1]] <- list()
          attributes(tree[[1]]) <- list(members = 1, height = 0, label = "0", leaf = TRUE)
          tree[[2]] <- list()
          attributes(tree[[2]]) <- list(members = 1, height = 0, label = "1", leaf = TRUE)
          
          return(tree)
        } else {
          # get dendrogram
          return(dendextend::find_dend(1 - corMat) %>%
                   ladderize)
        }
      })
      
      adataColDend <- reactive({
        req(heatmapType())
        req(adataColDendH())
        req(adataColDendC())
        
        dend <- NULL
        
        if (heatmapType() == "heatmap") {
          dend <- adataColDendH()
        } else if (heatmapType() == "correlation") {
          dend <- adataColDendC()
        }
        
        dend
      })
      
      # type of heatmap
      heatmapType <- reactive({
        req(input$heatmapType)
        
        input$heatmapType
      })
      
      # cluster order for heatmap dendrogram
      adataColDendLabels <- reactive({
        req(adataColDend())
        
        rev(adataColDend() %>% labels)
      })
      
      # number of k found based on correlation
      adataColDendK <- reactive({
        req(adataColDendC())
        
        # provide manual k if not enough clusters
        if (attr(adataColDendC(), "members") <= 2)
          2
        else
          dendextend::find_k(adataColDendC())$k
      })
      
      # attribute selection
      attrSelection <- reactive({
        req(imageData())
        
        # TODO anything else.. ?
        colnames(imageData())
      })
      
      # channel selection
      channelSelection <- reactive({
        req(cciaObj())
        req(cciaObj()$imChannelNames())
        
        # put sequence as values
        channelNames <- cciaObj()$imChannelNames(
          includeTypes = TRUE, correctChannelNames = TRUE)
        names(channelNames) <- unlist(cciaObj()$imChannelNames(
          includeTypes = TRUE))
        
        channelNames
      })
      
      ### Observers - RxAction
      ## Event specific
      
      # provide population type to input manager
      observeEvent(popTypeProcessing(), {
        req(popTypeProcessing())
        
        moduleManagers()$inputManager$setPopType(popTypeProcessing())
        
        # update inputs that depend on population type
        if (length(moduleManagers()$inputManager$getPopTypeInputs()) > 0) {
          cciaObj <- cciaObj()
          
          # get first image from set
          if (is.null(cciaObj)) {
            selectedUIDs <- moduleManagers()$selectionManager$selectedUIDs()
            
            req(length(selectedUIDs) > 0)
            
            cciaObj <- firstSelectedImageFromSet(
              selectedUIDs,
              moduleManagers()$imageSetManager$selectedSet()
            )
          } 
          
          for (x in moduleManagers()$inputManager$getPopTypeInputs()) {
            updateSelectInput(
              inputId = trimInputName(id, x),
              choices = if (!is.null(cciaObj$popUtils(popTypeProcessing())))
                cciaObj$popUtils(popType())$popPaths()
              else
                c("")
            )
          }
        }
      })
      
      # listen to data selection
      # this will also trigger when the same image is chosen again
      # selected - user selects
      # shown - the image shown
      observeEvent(moduleManagers()$imageViewerManager$imageSelected(), {
        req(cciaObj())
        req(clusterColName())
        
        # label properties
        labelProps(cciaObj()$labelProps()$as_df())
        
        # init anndata utils with reactivity
        # cciaObj()$adataUtils()$reactive()
        
        # set population DT
        adataDT(getAdataDT())
        
        # create adata matrix
        adataMat(adataMatFromPopDT(adataDT(), popKeys = clusterColName()))
        
        if (DEBUG_SHOW_VIEWER == TRUE && globalManagers$projectManager()$getProjectType() != "flow") {
          # init all populations
          clustSavePops(purge = TRUE)
          
          # save pop map
          cciaObj()$savePopMap(popType(), includeFiltered = TRUE)
        }
        
        # collapse selection box
        js$collapseBox(session$ns("imageTableBox"))
      })
      
      # set cluster for population
      observeEvent(setClusterForPop(), {
        req(setClusterForPop())
        req(cciaObj())
        
        # set cluster for population
        # split into population and cluster ID
        params <- stringr::str_split(
          setClusterForPop(), pattern = "\\.")[[1]]
        popID <- params[[1]]
        clustID <- as.numeric(params[[2]])
        
        popMap <- imPopMap()
        
        # was the cluster already set?
        addClustID <- TRUE
        if (clustID %in% popMap[[popID]]$filterValues) {
          addClustID <- FALSE
        }
        
        # remove cluster ID from list
        changedPops <- list()
        for (i in names(popMap)) {
          x <- popMap[[i]]
          
          # check whether cluster is set
          if (clustID %in% popMap[[i]]$filterValues) {
            changedPops[[i]] <- x$name
          }
          
          popMap[[i]]$filterValues <- popMap[[i]]$filterValues[popMap[[i]]$filterValues != clustID]
        }
        
        # add to population
        if (addClustID == TRUE) {
          popMap[[popID]]$filterValues <- c(
            unlist(popMap[[popID]]$filterValues), clustID
          )
          
          if (!popID %in% names(changedPops)) {
            changedPops[[popID]] <- popMap[[popID]]$name
          }
        }
        
        # set mapping
        cciaObj()$setImPopMap(popType(), popMap, mergeMap = TRUE)
        
        if (DEBUG_SHOW_VIEWER == TRUE && globalManagers$projectManager()$getProjectType() != "flow") {
          # save populations
          clustSavePops(changedPops)
          
          # save pop map
          cciaObj()$savePopMap(popType(), invalidate = FALSE, includeFiltered = TRUE)
        }
        
        # update manually to prevent redrawing of table
        # html(btnID, btnLabel)
        # go through all rows and disable, except chosen
        for (i in names(popMap)) {
          labelID <- sprintf(
            "setClusterForPop_%s_%s", clustID, i)
          
          if ("filterValues" %in% names(popMap[[i]]) &&
            clustID %in% popMap[[i]]$filterValues) {
            html(labelID, htmlIcon(btnICON_SELECTED))
          } else {
            html(labelID, htmlIcon(btnICON_NOTSELECTED))
          }
        }
        
        # udate viewer
        viewerUpdatePops(runif(1))
      })
      
      # show selected cells on image
      observeEvent({
        event_data("plotly_selected", source = "channelsPlot")
      }, {
        req(cciaObj())
        eventSelected <- event_data("plotly_selected", source = "channelsPlot")
        req(eventSelected)
        req(globalManagers$viewerManager()$viewer())
        
        # select cells on image
        globalManagers$viewerManager()$viewer()$highlightLabels(
          eventSelected$customdata
        )
      })
      
      # show channel intensities on image
      observeEvent(input$viewerShowChannelIntensity, {
        req(input$viewerShowChannelIntensity)
        req(globalManagers$viewerManager()$viewer())
        
        globalManagers$viewerManager()$viewer()$showChannelIntensity(
          which(channelSelection() == input$viewerShowChannelIntensity)[[1]]
        )
      }, ignoreInit = TRUE)
      
      # listen to viewer output
      observeEvent(globalManagers$viewerManager()$viewerOutput(), {
        req(cciaObj())
        
        viewerOutput <- globalManagers$viewerManager()$viewerOutput()
        outputProcessed <- FALSE
        
        # check whether there is something to do
        if ("clustPopulationsSelectPoints" %in% names(viewerOutput)) {
          if (length(viewerOutput$clustPopulationsSelectPoints)) {
            # set selected labels
            viewerSelectedLabels(viewerOutput$clustPopulationsSelectPoints)
            
            outputProcessed <- TRUE
          }
        }
        
        # tell the viewer that the command was processed
        if (outputProcessed == TRUE) {
          globalManagers$viewerManager()$clearViewerOutput()
        }
      })
      
      # renamed population
      observeEvent(moduleManagers()$populationManager$renamedPops(), {
        req(moduleManagers()$populationManager$renamedPops())
        
        # save pop map
        cciaObj()$savePopMap(popType(), includeFiltered = TRUE)
        
        # udate viewer
        viewerUpdatePops(runif(1))
      })
      
      # deleted population
      observeEvent(moduleManagers()$populationManager$deletedPops(), {
        req(moduleManagers()$populationManager$deletedPops())
        
        # delete from anndata
        cciaObj()$delPop(
          popType(),
          names(moduleManagers()$populationManager$deletedPops())
        )
        
        # save pop map
        cciaObj()$savePopMap(popType(), includeFiltered = TRUE)
        
        # udate viewer
        viewerUpdatePops(runif(1))
      })
      
      # added population
      observeEvent(moduleManagers()$populationManager$addedPops(), {
        req(moduleManagers()$populationManager$addedPops())
        
        pops <- moduleManagers()$populationManager$addedPops()
        
        # add path to populations
        for (i in names(pops)) {
          x <- pops[[i]]
          
          cciaObj()$setPopAttr(
            popType(), i, list(
              # TODO this does not work when there are multiple adata files
              # valueName = attr(cciaObj()$valueNamesForPopType(popType()), "default"),
              valueName = valueName(),
              # path = i,
              path = x$name,
              parent = "root",
              filterMeasure = clusterColName(),
              filterFun = "eq",
              filterValues = list(),
              isTrack = isTrack()
            ), includeFiltered = TRUE, invalidate = TRUE
          )
        }
        
        if (DEBUG_SHOW_VIEWER == TRUE && globalManagers$projectManager()$getProjectType() != "flow") {
          # save populations
          clustSavePops(moduleManagers()$populationManager$addedPops())
          
          # save pop map
          cciaObj()$savePopMap(popType(), includeFiltered = TRUE, invalidate = FALSE)
        }
        
        # udate viewer
        viewerUpdatePops(runif(1))
      })
      
      # listen to changes in heatmap
      observeEvent(heatmapType(), {
        req(cciaObj())
        
        # invalidate object to trigger table update
        cciaObj()$update()
      })
      
      # reset cell selection from image
      observeEvent(input$resetImageLabelSelection, {
        req(cciaObj())
        req(input$resetImageLabelSelection)
        
        viewerSelectedLabels(NULL)
      })
      
      # update population umap
      observeEvent(input$updateClustUMAPPops, {
        req(cciaObj())
        req(moduleManagers()$imageSetManager$selectedSet())
        
        # propagate cluster to associated images
        if (!is.null(clusteringPartOf())) {
          moduleManagers()$imageSetManager$selectedSet()$propagatePopMap(
            popType(),
            fromUID = cciaObj()$getUID(),
            toUIDs = clusteringPartOf()
          )
        }
        
        progress <- Progress$new()
        
        progress$set(
          message = "Update population map",
          value = 50)
        
        # update adata DT
        adataDT(getAdataDT(flushCache = TRUE))
        
        progress$close()
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
      
      ## Buttons
      output$popType <- renderUI({
        choices <- cciaConf()$parameters$popTypes
        
        selectInput(
          session$ns("popType"), "Population Type",
          choices = .reverseNamedList(choices),
          selected = choices[[globalManagers$projectManager()$getProjectType()]]
        )
      })
      
      output$valueName <- renderUI({
        req(cciaObj())
        req(popType())
        
        choices <- cciaObj()$valueNamesForPopType(popType())
        
        selectInput(
          session$ns("valueName"), "Value name",
          choices = choices,
          selected = attr(choices, "default"),
          multiple = FALSE
        )
      })
      
      output$clusterColName <- renderUI({
        req(cciaObj())
        req(popType())
        
        if (popType() == "live") {
          # this is only relevant for tracking
          # TODO why is this here?
          choices <- cciaObj()$labelPropsCols(
            # valueNames = c(valueName()),
            valueNames = valueName(),
            colsStartsWith = paste0(popType(), ".cell.track.clusters"))
        } else {
          choices <- c("clusters")
        }
        
        selectInput(
          session$ns("clusterColName"), "Cluster column",
          choices = choices,
          multiple = FALSE
        )
      })
      
      ## Other
      
      # selection for attributes in umap
      output$umapAttrSelection <- renderUI({
        req(attrSelection())
        
        createSelectInput(
          session$ns("umapAttrSelection"),
          "Select attribute",
          choices = attrSelection()[attrSelection() %in% names(adataDT())],
          selected = shinyInputValue("umapAttrSelection", input),
          multiple = FALSE
        )
      })
      
      # selection for markers in umap
      output$umapMarkerSelection <- renderUI({
        req(channelSelection())
        
        createSelectInput(
          session$ns("umapMarkerSelection"),
          "Select marker",
          choices = channelSelection()[channelSelection() %in% names(adataDT())],
          selected = shinyInputValue("umapMarkerSelection", input),
          multiple = FALSE
        )
      })
      
      # channel intensity selection
      output$viewerShowChannelIntensity <- renderUI({
        req(channelSelection())
        
        tagList(
          fluidRow(
            column(4, tags$label("Viewer")),
            column(
              8, 
              selectInput(
                session$ns("viewerShowChannelIntensity"),
                NULL,
                channelSelection(),
                selected = input$viewerShowChannelIntensity
              )
            )
          )
        )
      })
      
      # channel plot
      output$channelsPlot <- renderPlotly({
        req(labelProps())
        req(input$channelsPlotX)
        req(input$channelsPlotY)
        
        axisLabels <- lapply(
          c(input$channelsPlotX, input$channelsPlotY),
          function(x) names(channelSelection())[channelSelection() == x]
          )
        
        # highlight cells
        colours <- list(
          colours = NULL,
          order = NULL
        )
        
        if (!is.null(viewerSelectedLabels())) {
          colours$colours <- rep(cciaConf()$fcs$gating$default$colour, nrow(labelProps()))
          colours$colours[labelProps()$label %in% viewerSelectedLabels()] <- cciaConf()$fcs$gating$highlight$colour
          
          # set order
          colours$order <- c(
            cciaConf()$fcs$gating$default$colour,
            cciaConf()$fcs$gating$highlight$colour
          )
        }
        
        # create plotly plot
        moduleManagers()$uiManager$flowPlot_ly("channelsPlot", enableGating = FALSE) %>%
          moduleManagers()$uiManager$flowPlot_lyTraces(
            labelProps(), input$channelsPlotX, input$channelsPlotY,
            customdata = labelProps()$label, colours = colours$colours,
            coloursOrder = colours$order
          ) %>%
          moduleManagers()$uiManager$flowPlot_lyLayout(
            xlab = axisLabels[[1]], ylab = axisLabels[[2]], enableGating = FALSE
          ) %>%
          toWebGL()
      })
      
      # channel plot selection X and Y
      output$channelsPlotX <- renderUI({
        req(channelSelection())
        
        tagList(
          fluidRow(
            column(4, tags$label("X")),
            column(
              8, 
              selectInput(
                session$ns("channelsPlotX"), NULL, channelSelection(),
                selected = input$channelsPlotX)
            )
          )
        )
      })
      
      output$channelsPlotY <- renderUI({
        req(channelSelection())
        
        tagList(
          fluidRow(
            column(4, tags$label("Y")),
            column(
              8, 
              selectInput(
                session$ns("channelsPlotY"), NULL, channelSelection(),
                selected = input$channelsPlotY)
            )
          )
        )
      })
      
      # uamp plots
      output$clustUMAPClusters <- renderPlotly({
        req(adataDT())
        
        plotUMAP(clusterColName(), "clustUMAPClusters")
      })
      
      output$clustUMAPPops <- renderPlotly({
        req(adataDT())
        
        # TODO Or do you want this to happen every time a population
        # is changing?
        isolate({
          plotUMAP("pop", "clustUMAPPops",
                   plotType = "pops")
        })
      })
      
      output$clustUMAPMarkers <- renderPlotly({
        req(adataDT())
        req(umapMarkerSelection())
        
        plotUMAP(umapMarkerSelection(), "clustUMAPMarkers",
                 plotType = "intensity")
      })
      
      output$clustUMAPAttrs <- renderPlotly({
        req(adataDT())
        req(umapAttrSelection())
        
        plotUMAP(umapAttrSelection(), "clustUMAPAttrs",
                 plotType = "intensity")
      })
      
      # heatmap plot
      output$heatmapPlot <- renderPlotly({
        req(adataMat())
        req(adataColDendH())
        req(adataColDendK())
        
        # define order of identified clusters
        heatmaply(
          adataMat(),
          hide_colorbar = TRUE,
          Colv = adataColDendH(),
          k_col = adataColDendK()
          ) %>%
          layout(
            plot_bgcolor = "#222",
            paper_bgcolor = "#222",
            font = list(color = '#d3d3d3'),
            xaxis = plotlyAx,
            yaxis2 = plotlyAx
          )
      })
      
      # correlation plot
      output$correlationPlot <- renderPlotly({
        req(adataMat())
        req(adataColDendC())
        req(adataColDendK())
        
        heatmaply_cor(
          cor(adataMat()),
          hide_colorbar = TRUE,
          xlab = "",
          ylab = "",
          Rowv = adataColDendC(),
          Colv = adataColDendC(),
          k_col = adataColDendK(),
          k_row = adataColDendK()
        ) %>%
          layout(
            plot_bgcolor = "#222",
            paper_bgcolor = "#222",
            font = list(color = '#d3d3d3'),
            xaxis = plotlyAx,
            yaxis2 = plotlyAx
          )
      })
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet",
        "imageViewer", "population", "shapes")
      managerConf = list(
        moduleName = id,
        selectionData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "clust_populations"
        ),
        task = list(
          funLabel = "Mapping method",
          taskVarsToAdd = taskVarsToAdd,
          runTaskCombinedSelect = TRUE
        ),
        population = list(
          imPopMap = imPopMap,
          popData = popData,
          popType = popType,
          updateImage = updateImage,
          enableFilterPopulation = TRUE,
          enableEditPopName = TRUE,
          popTable = list(
            ordering = FALSE,
            dom = "ti",
            pageLength = -1
          )
        ),
        shapes = list(
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
