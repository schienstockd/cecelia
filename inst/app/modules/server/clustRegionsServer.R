#' @description Server to clust populations
#' @importFrom plotly renderPlotly
#' 
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.clustRegionsServer <- function(id, parent, globalManagers) {
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
          popType(), pops,
          purge = purge,
          includeFiltered = TRUE
          )
      }
      
      # get adata DT
      getAdataDT <- function(flushCache = FALSE) {
        if (!is.null(regionPartOf())) {
          # get objects from selected set
          moduleManagers()$imageSetManager$selectedSet()$popDT(
            popType = popType(),
            uIDs = regionPartOf(),
            includeFiltered = TRUE,
            completeDT = FALSE, replaceNA = TRUE,
            flushCache = flushCache,
            # only focus on clustered values
            filterMeasures = c("regions")
          )
        } else {
          # anndata
          cciaObj()$popDT(popType(), includeFiltered = TRUE,
                          completeDT = FALSE, replaceNA = TRUE,
                          # only focus on clustered values
                          filterMeasures = c("regions"))
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
      popType <- reactive("region")
      popTypeProcessing <- reactive({
        input$popType
      })
      
      # add pop type to task variables
      taskVarsToAdd <- reactive({
        req(popTypeProcessing())
        
        list(
          popType = popTypeProcessing()
        )
      })
      
      # set clustering part of
      regionPartOf <- reactive({
        req(cciaObj())
        
        cciaObj()$valuePartOf("imAnndataFilepath")
      })
      
      setRegionForPop <- reactive({
        req(input$setRegionForPop)
        
        input$setRegionForPop
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
        cciaObj()
        ), {
        req(cciaObj())
        
        cciaObj()$imPopMap(popType(), includeFiltered = TRUE, filterMeasures = c("regions"))
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
        regionCols <- list()
        inputID <- "setRegionForPop"
        
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
            regionCols <- append(regionCols, curCol)
          }
        }
        
        # create dataframe
        popsDFwithRegions <- do.call(cbind, regionCols)
        
        if (!is.null(popsDFwithRegions)) {
          names(popsDFwithRegions) <- colnames(popsDF)
          colnames(popsDFwithRegions) <- colnames(popsDF)
          rownames(popsDFwithRegions) <- rownames(popsDF)
        }
        
        popsDFwithRegions
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
      
      # dendrograms for regions
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
        
        # create dendrogram if only two regions were found
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
      
      # region order for heatmap dendrogram
      adataColDendLabels <- reactive({
        req(adataColDend())
        
        rev(adataColDend() %>% labels)
      })
      
      # number of k found based on correlation
      adataColDendK <- reactive({
        req(adataColDendC())
        
        # provide manual k if not enough regions
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
                cciaObj$popUtils(popTypeProcessing())$popPaths()
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
        
        # label properties
        labelProps(cciaObj()$labelProps()$as_df())
        
        # init anndata utils with reactivity
        # cciaObj()$adataUtils()$reactive()
        
        if (DEBUG_SHOW_VIEWER == TRUE && globalManagers$projectManager()$getProjectType() != "flow") {
          # init all populations
          clustSavePops(purge = TRUE)
          
          # save pop map
          cciaObj()$savePopMap(popType(), includeFiltered = TRUE)
        }
        
        # set population DT
        adataDT(getAdataDT())
        
        # create adata matrix
        adataMat(adataMatFromPopDT(adataDT(), popKeys = "regions"))
        
        # collapse selection box
        js$collapseBox(session$ns("imageTableBox"))
      })
      
      # set region for population
      observeEvent(setRegionForPop(), {
        req(setRegionForPop())
        req(cciaObj())
        
        # set region for population
        # split into population and region ID
        params <- stringr::str_split(
          setRegionForPop(), pattern = "\\.")[[1]]
        popID <- params[[1]]
        clustID <- as.numeric(params[[2]])
        
        popMap <- imPopMap()
        
        # was the region already set?
        addClustID <- TRUE
        if (clustID %in% popMap[[popID]]$filterValues) {
          addClustID <- FALSE
        }
        
        # remove region ID from list
        changedPops <- list()
        for (i in names(popMap)) {
          x <- popMap[[i]]
          
          # check whether region is set
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
            "setRegionForPop_%s_%s", clustID, i)
          
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
              valueName = attr(cciaObj()$valueNamesForPopType(popType()), "default"),
              # path = i,
              path = x$name,
              parent = "root",
              filterMeasure = "regions",
              filterFun = "eq",
              filterValues = list()
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
      
      ## Other
      
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
        imageData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "clust_regions"
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
