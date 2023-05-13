# manage plot for flow
createFlowPlotManager <- function(
  input, output, session, globalManagers, moduleManagers, managerConf) {
  # set default parameters
  if (!"enableGatePopulation" %in% names(managerConf$flowPlot)) {
    managerConf$flowPlot$enableGatePopulation <- FALSE
  }

  if (!"showFilters" %in% names(managerConf$flowPlot)) {
    managerConf$flowPlot$showFilters <- FALSE
  }

  ###
  # DEBUG
  ###
  debugPlotsRendered <- function() {
    flowGatingPlotsShown(numFlowPlots())
  }

  ### Functions
  # create a 'gating box'
  createGatingBox <- function(flowGatingPlot, width = 6) {
    # get ids
    boxIDs <- flowGatingPlot()$getBoxIDs()

    box(
      solidHeader = TRUE,
      collapsible = FALSE,
      title = flowGatingPlot()$getPlotName(),
      status = "primary",
      width = width,
      fluidRow(
        column(
          12,
          plotlyOutput(session$ns(boxIDs$plot), height = "500px"),
          tags$hr(),
          fluidRow(
            column(
              4,
              createGatingBoxChannelSelect(
                boxIDs$axisX, "X",
                flowGatingPlot()$getPlotXchannel(),
                ignoreInput = initGatingBoxPlots()),
              createGatingBoxChannelSelect(
                boxIDs$axisY, "Y",
                flowGatingPlot()$getPlotYchannel(),
                ignoreInput = initGatingBoxPlots())
            ),
            column(
              2,
              createGatingBoxChannelScaleSelect(
                boxIDs$axisXScale, "Scale",
                flowGatingPlot()$getPlotXchannelScale(),
                ignoreInput = initGatingBoxPlots()),
              createGatingBoxChannelScaleSelect(
                boxIDs$axisYScale, "Scale",
                flowGatingPlot()$getPlotYchannelScale(),
                ignoreInput = initGatingBoxPlots())
            ),
            column(
              6,
              uiOutput(session$ns(boxIDs$popPath)),
              uiOutput(session$ns(boxIDs$popLeaves))
            )
          ),
          fluidRow(
            column(
              3, uiOutput(session$ns(boxIDs$plotType))
              )
          )
          # fluidRow(
          #   column(
          #     6, uiOutput(session$ns(boxIDs$filterMeasure))
          #   ),
          #   column(
          #     6, uiOutput(session$ns(boxIDs$filterValues))
          #   )
          # )
        )
      )
    )
  }

  # get ids for gating box
  flowGatingBoxIDs <- function(i, convertID = FALSE) {
    # convert ID
    if (convertID == TRUE) {
      i <- stringr::str_extract(i, "(?<=_)[0-9]+$")
    }

    # create list
    list(
      i = as.numeric(i),
      plot = paste("gatingBoxPlot", flowPlotUID(), i, sep = "_"),
      axisX = paste("gatingBoxAxisX", flowPlotUID(), i, sep = "_"),
      axisY = paste("gatingBoxAxisY", flowPlotUID(), i, sep = "_"),
      axisXScale = paste("gatingBoxAxisXScale", flowPlotUID(), i, sep = "_"),
      axisYScale = paste("gatingBoxAxisYScale", flowPlotUID(), i, sep = "_"),
      popPath = paste("gatingBoxPopSelect", flowPlotUID(), i, sep = "_"),
      popLeaves = paste("gatingBoxPopLeaves", flowPlotUID(), i, sep = "_"),
      filterMeasure = paste("gatingBoxFilterMeasure", flowPlotUID(), i, sep = "_"),
      filterValues = paste("gatingBoxFilterValues", flowPlotUID(), i, sep = "_"),
      plotType = paste("gatingBoxPlotType", flowPlotUID(), i, sep = "_")
    )
  }

  # create gating plot
  createGatingBoxPlot <- function(flowGatingPlot) {
    x <- flowGatingPlot()$getPlotXchannel()
    y <- flowGatingPlot()$getPlotYchannel()
    xScale <- flowGatingPlot()$getPlotXchannelScale()
    yScale <- flowGatingPlot()$getPlotYchannelScale()

    # highlighted cells?
    # otherwise the plot will be reloaded after changing traces
    isolate({
      boxIDs <- flowGatingPlot()$getBoxIDs()

      # define populations to plot
      popsToPlot <- c(
        flowGatingPlot()$getPlotPopPath(),
        flowGatingPlot()$getPlotPopLeaves()
      )

      # get pop data tables and colours
      popsPrep <- flowGatingPlotPrepPops(
        popsToPlot, flowGatingPlot,
        greyPops = flowGatingPlot()$getPlotPopPath())
      
      # get limits
      channelLimits <- flowChannelLimits(flowGatingPlot)
      
      # if centroid X and Y - use fixed aspect ratio
      axisScale <- NULL
      if (all(c(x, y) %in% c("centroid_x", "centroid_y"))) {
        axisScale <- list(
          y = list(anchor = "x", ratio = 1)
        )
      }
      
      # prepare raster image
      if (flowGatingPlot()$getPlotType() == "raster") {
        # set axis scale because it is an image
        axisScale <- list(
          x = list(
            anchor = "x",
            ratio = diff(channelLimits$y)/diff(channelLimits$x)
          ),
          y = list(
            anchor = "x",
            ratio = diff(channelLimits$x)/diff(channelLimits$y)
          )
        )
      }
      
      # create plotly plot
      p1 <- moduleManagers()$uiManager$flowPlot_ly(
        boxIDs$plot,
        screenshotConf = list(
          height = input$plotlyScreenshotHeight,
          width = input$plotlyScreenshotHeight
        )
      ) %>%
        moduleManagers()$uiManager$flowPlot_lyLayout(
          xlab = x, ylab = y,
          xlim = channelLimits$x,
          ylim = channelLimits$y,
          xScale = xScale,
          yScale = yScale,
          axisScale = axisScale,
          reverseRange = flowReverseRangeForAxis(flowGatingPlot)
        )
      
      # add populations
      for (pop in popsPrep) {
        # make sure x and y are in DT
        x <- .flowCorrectChannelNames(x)
        y <- .flowCorrectChannelNames(y)
        
        xOK <- x %in% colnames(pop$dt)
        yOK <- y %in% colnames(pop$dt)

        if (all(xOK, yOK)) {
          p1 <- p1 %>% moduleManagers()$uiManager$flowPlot_lyTraces(
            pop$dt, x, y, colours = pop$colours$colours,
            # customdata = paste(pop$dt$value_name, pop$dt$label, sep = "#"),
            coloursOrder = pop$colours$order,
            useFlowColours = if (flowGatingPlot()$getPlotType() != "contour") flowUseFlowColours() else FALSE,
            markerOpacity = if (flowUseFlowColours() == TRUE) 1 else flowMarkerOpacity(),
            plotType = flowGatingPlot()$getPlotType()
          )
        }
      }
      
      p1 %>% event_register("plotly_afterplot") %>% toWebGL()
    })
  }

  # reverse range for centroids
  flowReverseRangeForAxis <- function(flowGatingPlot) {
    reverseRange <- NULL

    channelAxis <- c(
      flowGatingPlot()$getPlotXchannel(),
      flowGatingPlot()$getPlotYchannel()
    )
    
    if ("centroid_y" %in% channelAxis) {
      reverseRange <- c("x", "y")[which("centroid_y" == channelAxis)]
    }

    reverseRange
  }

  # return channel limits
  flowChannelLimits <- function(flowGatingPlot) {
    flowX <- flowGatingPlot()$getPlotXchannel(flowName = TRUE)
    flowY <- flowGatingPlot()$getPlotYchannel(flowName = TRUE)
    
    # get root for channels
    flowDT <- cciaObj()$popDT(
      popType(), pops = c("root"), includeFiltered = FALSE,
      completeDT = FALSE, popCols = c(flowX, flowY))
    
    # get channel limits
    # channelLimits <- cciaObj()$popUtils(popType())$getImChannelLimits()
    
    xlim <- range(flowDT[[flowX]])
    ylim <- range(flowDT[[flowY]])

    list(x = xlim, y = ylim)
  }

  # create channel selection
  createGatingBoxChannelSelect <- function(id, label, selected, ignoreInput = FALSE) {
    # add properties
    # chnlNames <- names(cciaObj()$popUtils(popType())$getImChannelLimits())
    # chnlNames <- c(unname(cciaObj()$imChannelNames()),
    chnlNames <- c(unname(cciaObj()$imChannelNames(includeTypes = TRUE)),
                   cciaConf()$fcs$propsToAdd)
    
    selectInput(
      session$ns(id), label, chnlNames,
      selected = shinyInputValue(id, input, selected, ignoreInput = ignoreInput)
    )
  }

  # create channel scale selection
  createGatingBoxChannelScaleSelect <- function(id, label, selected, ignoreInput = FALSE) {
    selectInput(
      session$ns(id), label, c("linear", "log"),
      selected = shinyInputValue(id, input, selected, ignoreInput = ignoreInput)
    )
  }

  # create populations selection
  createGatingBoxPopSelect <- function(id, items, selected, ignoreInput = FALSE) {
    id <- flowGatingBoxIDs(id)$popPath

    selectInput(
      session$ns(id), "Population", items,
      selected = shinyInputValue(id, input, selected, ignoreInput = ignoreInput)
    )
  }

  # create leaves selection
  createGatingBoxPopLeaves <- function(id, items, selected, ignoreInput = FALSE) {
    id <- flowGatingBoxIDs(id)$popLeaves

    selectInput(
      session$ns(id), "Subpopulations", items,
      selected = shinyInputValue(id, input, selected, ignoreInput = ignoreInput),
      multiple = TRUE
    )
  }

  # create plot type selection
  createGatingBoxPlotType <- function(id, selected, ignoreInput = FALSE) {
    id <- flowGatingBoxIDs(id)$plotType

    selectInput(
      session$ns(id), "Type", .reverseNamedList(cciaConf()$fcs$gating$plotTypes),
      selected = shinyInputValue(id, input, selected, ignoreInput = ignoreInput),
      multiple = FALSE
    )
  }

  # create filter measure selection
  createGatingBoxFilterMeasures <- function(id, items, selected, ignoreInput = FALSE) {
    id <- flowGatingBoxIDs(id)$filterMeasure

    selectInput(
      session$ns(id), "Filter", items,
      selected = shinyInputValue(id, input, selected, ignoreInput = ignoreInput),
      multiple = FALSE
    )
  }

  # create filter value selection
  createGatingBoxFilterValues <- function(id, items, selected, ignoreInput = FALSE) {
    id <- flowGatingBoxIDs(id)$filterValues

    selectInput(
      session$ns(id), "Values", items,
      selected = shinyInputValue(id, input, selected, ignoreInput = ignoreInput),
      multiple = FALSE
    )
  }

  # save populations
  flowSavePops <- function(pops = NULL, purge = FALSE, filteredOnly = FALSE,
                           includeFiltered = FALSE, completeDT = TRUE) {
    if (!is.null(cciaObj()$popUtils(popType()))) {
      # get only filtered pops
      if (filteredOnly == TRUE) {
        pops <- cciaObj()$popPaths(popType(), filteredOnly = TRUE)
      }

      # save
      cciaObj()$savePops(popType(), pops = pops,
                         includeFiltered = includeFiltered,
                         purge = purge, completeDT = completeDT)
    }
  }

  # delete populations
  flowUnlinkPops <- function(popPath, popsToAdd = NULL) {
    if (!is.null(cciaObj()$popUtils(popType()))) {
      cciaObj()$popUtils(popType())$unlinkPops(
        flowPopList(popPath, popsToAdd = popsToAdd),
        file.path(
          cciaObj()$persistentObjectDirectory(),
          cciaConf()$dirs$tasks$populations, popType()))
    }
  }

  # create list of populations with IDs
  flowPopList <- function(pop, popsToAdd = NULL) {
    pop <- if (pop == "root") "/" else pop

    # get pop ids with that population as parent
    popIDs <- cciaObj()$popIDsByAttr(
      popType(), "path", pop, compareFun = "startsWith"
    )

    # get all leaves of population
    popsInfo <- cciaObj()$imPopMap(popType(), popIDs = popIDs)

    # add pops that have already been deleted
    popsInfo <- append(
      popsInfo,
      popsToAdd
    )

    pops <- lapply(popsInfo, function(x) x$path)
    names(pops) <- names(popsInfo)

    pops
  }

  # update pop leaves
  flowUpdatePopLeaves <- function(flowPlot, popLeavesToUpdate) {
    if (length(popLeavesToUpdate) > 0) {
      boxIDs <- flowPlot()$getBoxIDs()

      # add popIDs if not given
      if (is.null(names(popLeavesToUpdate))) {
        names(popLeavesToUpdate) <- cciaObj()$popIDsByAttr(
          popType(), "path", popLeavesToUpdate,
          compareFun = "in", includeFiltered = TRUE
        )
      }

      # get trace ID for population
      traceIDs <- expandTraceIDs(
        which(flowPlot()$getPlotPopLeaves() %in% popLeavesToUpdate),
        flowPlot()$getPlotType()
      )

      # append pop path to the end
      newLeaves <- flowPlot()$getPlotPopLeaves()
      newLeaves <- newLeaves[!newLeaves %in% popLeavesToUpdate]
      newLeaves <- c(newLeaves, popLeavesToUpdate)

      # update trace if subpopulations are shown
      flowGatingPlotChangeTrace(flowPlot, redrawIfLeaves = TRUE,
                                leavesOnly = TRUE, removeTraceIDs = traceIDs,
                                popsToPlot = popLeavesToUpdate)

      # update trace IDs
      flowPlot()$setPlotPopLeaves(newLeaves, invalidate = FALSE)
    }
  }

  # change traces for gating plot
  flowGatingPlotChangeTrace <- function(flowGatingPlot,
                                        leavesOnly = FALSE, removeTraceIDs = c(0),
                                        removeTraceNum = 0,
                                        popsToPlot = c(), ignoreNullPops = FALSE,
                                        redrawIfLeaves = FALSE, forceRedraw = FALSE) {
    boxIDs <- flowGatingPlot()$getBoxIDs()

    # are there leaves for plotting?
    numLeaves <- length(flowGatingPlot()$getPlotPopLeaves())

    # redraw for leaves?
    redrawIfLeaves <- redrawIfLeaves == TRUE && numLeaves > 0

    # remove all traces if watching leaves
    if (redrawIfLeaves == TRUE || forceRedraw == TRUE) {
      # all leaves and parent and delete
      removeTraceNum <- numLeaves + 1
    }

    if (is.null(popsToPlot) && ignoreNullPops == FALSE) {
      # add parent
      if (leavesOnly == FALSE || redrawIfLeaves == TRUE || forceRedraw == TRUE) {
        popsToPlot <- c(popsToPlot, flowGatingPlot()$getPlotPopPath())
      }

      # add leaves
      popsToPlot <- c(popsToPlot, flowGatingPlot()$getPlotPopLeaves())
    }

    # remove traces
    if (length(removeTraceIDs) > 0) {
      moduleManagers()$uiManager$flowPlot_lyRemoveTraces(boxIDs$plot, traceIDs = removeTraceIDs)
    } else if (removeTraceNum > 0) {
      moduleManagers()$uiManager$flowPlot_lyRemoveTraces(boxIDs$plot, n = removeTraceNum)
    }

    # get pop data tables and colours
    popsPrep <- flowGatingPlotPrepPops(
      popsToPlot, flowGatingPlot,
      greyPops = flowGatingPlot()$getPlotPopPath())
    
    # get channel limits
    channelLimits <- flowChannelLimits(flowGatingPlot)

    progress <- Progress$new()

    progress$set(message = "Change traces ... ", value = 50)

    # go through populations selected
    for (x in popsPrep) {
      # change trace
      moduleManagers()$uiManager$flowPlot_lyChangeTrace(
        boxIDs$plot, x$dt,
        flowGatingPlot()$getPlotXchannel(),
        flowGatingPlot()$getPlotYchannel(),
        xScale = flowGatingPlot()$getPlotXchannelScale(),
        yScale = flowGatingPlot()$getPlotYchannelScale(),
        xlim = channelLimits$x,
        ylim = channelLimits$y,
        # customdata = paste(x$dt$value_name, x$dt$label, sep = "#"),
        colours = x$colours$colours,
        coloursOrder = x$colours$order,
        removeTraces = FALSE,
        useFlowColours = if (flowGatingPlot()$getPlotType() != "contour") flowUseFlowColours() else FALSE,
        markerOpacity = if (flowUseFlowColours() == TRUE) 1 else flowMarkerOpacity(),
        reverseRange = flowReverseRangeForAxis(flowGatingPlot),
        plotType = flowGatingPlot()$getPlotType()
      )
    }

    progress$close()
  }

  # prepare populations for plotting
  flowGatingPlotPrepPops <- function(popsToPlot, flowGatingPlot,
                                     greyPops = c("root", "/", "")) {
    pops <- list()

    # only plot the first population if there
    # are cells selected in the viewer
    if (!is.null(viewerSelectedLabels())) {
      popsToPlot <- popsToPlot[1]
    }

    # get pop info
    popsInfo <- cciaObj()$imPopMap(
      popType(), popIDs = names(popsToPlot))
    
    # get population DT
    # TODO you should only get the channels that you need
    DT <- cciaObj()$popDT(
      popType(), pops = popsToPlot, includeFiltered = TRUE,
      completeDT = FALSE, popCols = c(
        .flowCorrectChannelNames(flowGatingPlot()$getPlotXchannel()),
        .flowCorrectChannelNames(flowGatingPlot()$getPlotYchannel())
      ))
    
    if (!is.null(DT)) {
      # go through pops and create list of datatable and colours
      for (x in names(popsToPlot)) {
        pops[[x]] <- list()
        popInfo <- popsInfo[[x]]
  
        # filter on population in DT
        if (.flowPopIsRoot(x)) {
          popDT <- DT[]
        } else {
          # is the population defined?
          if (is.null(popInfo)) {
            popDT <- DT[pop == x,]
          } else {
            popDT <- DT[pop == popInfo$path,]
          }
        }
  
        # filter cells from selection on plot?
        if (length(flowGatingPlot()$getPlotFilterLabels()) > 0) {
          popDT <- popDT[label %in% flowGatingPlot()$getPlotFilterLabels()]
        }
  
        # highlight cells?
        colours <- flowGatingPlotHighlightCells(popDT)
  
        # are there highlighted cells?
        if (is.null(colours$colours)) {
          # are there any leaves selected?
          if (length(flowGatingPlot()$getPlotPopLeaves()) > 0) {
            # if (cciaObj()$popUtils(popType())$popIsRoot(x)) {
            if (any(x %in% names(greyPops))) {
              colours$colours <- cciaConf()$fcs$gating$default$colour
            } else {
              # get colour for current population
              colours$colours <- popInfo$colour
            }
          }
        }
  
        # save in list
        pops[[x]]$dt <- popDT
        pops[[x]]$colours <- colours
      }
    }

    pops
  }

  # get colours for highlighted cells
  flowGatingPlotHighlightCells <- function(dt) {
    colours <- list(
      colours = NULL,
      order = NULL
    )

    if (!is.null(viewerSelectedLabels())) {
      colours$colours <- rep(cciaConf()$fcs$gating$default$colour, nrow(dt))
      colours$colours[which(dt$label %in% viewerSelectedLabels())] <- cciaConf()$fcs$gating$highlight$colour

      # set order
      colours$order <- c(
        cciaConf()$fcs$gating$default$colour,
        cciaConf()$fcs$gating$highlight$colour
      )
    }

    colours
  }

  # save gating plot values to image
  flowSavePopGatePlot <- function(flowGatingPlot, invalidate = TRUE) {
    cciaObj()$setImPopGatePlot(
      popType(), flowGatingPlot()$getBoxIDs()$i, list(
        x = flowGatingPlot()$getPlotXchannel(),
        y = flowGatingPlot()$getPlotYchannel(),
        xScale = flowGatingPlot()$getPlotXchannelScale(),
        yScale = flowGatingPlot()$getPlotYchannelScale(),
        popPath = flowGatingPlot()$getPlotPopPath(),
        popLeaves = flowGatingPlot()$getPlotPopLeaves(),
        filterMeasure = flowGatingPlot()$getPlotFilterMeasure(),
        filterValues = flowGatingPlot()$getPlotFilterValues(),
        plotType = flowGatingPlot()$getPlotType()
      ), invalidate = invalidate)
  }

  # has the flow plot changed?
  flowPlotChanged <- function(inputID, flowPlotValues) {
    curInput <- input[[inputID]]
    retVal <- NULL

    # get differences
    dif <- union(
      setdiff(curInput, flowPlotValues), setdiff(flowPlotValues, curInput))

    if (xor(purrr::is_empty(flowPlotValues), purrr::is_empty(curInput)) || length(dif) > 0) {
      retVal <- list(
        input = curInput,
        dif = dif
      )
    }

    retVal
  }

  # expand trace ids for plotting type
  expandTraceIDs <- function(traceIDs, plotType) {
    if (plotType == "contour") {
			# trace IDs should be multiplied by '2' to account
      # for contour and outliers
      traceIDs <- traceIDs * 2
      traceIDs <- c(traceIDs, traceIDs + 1)
    }

    traceIDs
  }

  ### Reactive values
  # ccia object
  cciaObj <- reactive({
    managerConf$cciaObj()
  })

  # population type that is used in for the plot
  popType <- reactive({
    managerConf$flowPlot$popType()
  })

  # number of shown plots
  numFlowPlots <- reactive({
    managerConf$flowPlot$numFlowPlots()
  }) %>% debounce(cciaConf()$fcs$gating$plots$poll)

  # use flow colours?
  flowUseFlowColours <- reactive({
    managerConf$flowPlot$flowUseFlowColours()
  })

  # marker opacity
  flowMarkerOpacity <- reactive({
    managerConf$flowPlot$flowMarkerOpacity()
  })

  # general
  initGatingBoxPlots <- reactiveVal(TRUE)
  flowPopLeavesUpdated <- reactiveVal(NULL)

  # list of gating plots
  flowGatingPlotsShown <- reactiveVal(0)
  flowGatingPlots <- reactiveVal()
  flowGatingBoxes <- reactiveVal()

  # generate ui for plots shared in the same module
  flowPlotUID <- reactiveVal(genUID(4))

  # viewer
  viewerSelectedLabels <- reactiveVal()

  # when updating the plot objects
  # set this to FALSE
  flowAfterplot <- reactiveVal(TRUE)

  # directory for populations
  flowPopDir <- reactive({
    req(cciaObj())

    file.path(
      cciaObj()$persistentObjectDirectory(),
      cciaConf()$dirs$tasks$populations, popType())
  })

  # check that plots finished rendering
  flowGatingPlotsRendered <- eventReactive(c(
    numFlowPlots(),
    flowGatingPlotsShown()
  ), {
    req(cciaObj())

    flowGatingPlotsShown() == numFlowPlots()
  })

  # prepare plot events
  flowGatingPlotPrepEvents <- function(plotEvents) {
    # set names
    names(plotEvents) <- flowGatingPlotSourceIDs()

    # is there anything?
    for (i in names(plotEvents)) {
      if (is.null(plotEvents[[i]])) {
        plotEvents[[i]] <- NULL
      }
    }

    # TODO How can I catch which was the trigger?
    # if (length(plotEvents) > 0) {
    #   # set triggers
    #   triggerEvents <- lapply(flowGatingPlots(), function(x) x()$isPlotEventTriggered())
    #   setattr(plotEvents, "trigger", names(plotEvents)[unlist(triggerEvents)])
    # }

    plotEvents
  }

  # get plot source ids
  flowGatingPlotSourceIDs <- reactive({
    lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$plot)
  })

  # get selected events for plots
  flowGatingPlotsSelected <- reactive({
    req(flowAfterplot())

    # get events
    plotEvents <- lapply(flowGatingPlots(), function(x) x()$plotReactiveSelected())

    flowGatingPlotPrepEvents(plotEvents)
  })

  # get afterplot events for plots
  flowGatingPlotsAfterplot <- reactive({
    # get events
    plotEvents <- lapply(flowGatingPlots(), function(x) x()$plotReactiveAfterplot())

    flowGatingPlotPrepEvents(plotEvents)
  })

  # get relayout events for plots
  flowGatingPlotsRelayout <- reactive({
    # get events
    plotEvents <- lapply(flowGatingPlots(), function(x) x()$plotReactiveRelayout())

    flowGatingPlotPrepEvents(plotEvents)
  })

  # get hover events for plots
  flowGatingPlotsHover <- reactive({
    # get events
    plotEvents <- lapply(flowGatingPlots(), function(x) x()$plotReactiveHover())

    flowGatingPlotPrepEvents(plotEvents)
  })

  # listen to population changes
  flowGatingPlotsPopChanges <- reactive({
    observeInputs(
      unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$popPath)),
      input
    )
  })

  # listen to leave changes
  flowGatingPlotsLeaveChanges <- reactive({
    observeInputs(
      unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$popLeaves)),
      input
    )
  })

  # listen to filter measure changes
  flowGatingPlotsFilterMeasureChanges <- reactive({
    req(managerConf$flowPlot$showFilters)

    observeInputs(
      unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$filterMeasure)),
      input
    )
  })

  # listen to filter value changes
  flowGatingPlotsFilterValuesChanges <- reactive({
    req(managerConf$flowPlot$showFilters)

    observeInputs(
      unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$filterValues)),
      input
    )
  })

  # listen to plot type changes
  flowGatingPlotsTypeChanges <- reactive({
    c(
      observeInputs(
        unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$plotType)),
        input
      )
    )
  })

  # listen to axis changes
  flowGatingPlotsAxisChanges <- reactive({
    c(
      observeInputs(
        unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$axisX)),
        input
      ),
      observeInputs(
        unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$axisY)),
        input
      )
    )
  })

  # listen to axis scale changes
  flowGatingPlotsAxisScaleChanges <- reactive({
    c(
      observeInputs(
        unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$axisXScale)),
        input
      ),
      observeInputs(
        unlist(lapply(flowGatingPlots(), function(x) x()$getBoxIDs()$axisYScale)),
        input
      )
    )
  })

  # population stats
  flowPopStats <- eventReactive(c(
    moduleManagers()$imageViewerManager$imageSelected()
  ), {
    req(cciaObj())

    cciaObj()$popStats(popType())
  })

  ### Reactive-like values

  ### Reactives - RxCalc
  ## Event specific

  ## Generic

  ### Observers - RxAction
  ## Event specific

  # render plots
  observeEvent(flowGatingBoxes(), {
    # go through plots
    for (x in flowGatingPlots()) {
      # https://gist.github.com/wch/5436415/
      local({
        # copy variables to local environment
        # otherwise the values will the same across
        local_x <- x
        local_boxIDs <- local_x()$getBoxIDs()

        # set population selection
        output[[local_boxIDs$popPath]] <- renderUI({
          req(cciaObj())

          # get the population paths from the pop map
          # because, filtered populations are not in the
          # population utils
          # cciaObj()$popUtils(popType())$popPaths(),
          # popMapPaths <- cciaObj()$popAttr(
          #   popType(), "path",
          #   includeFiltered = managerConf$flowPlot$showFilters)
          #
          # if (length(popMapPaths) > 0) {
          #   popPaths <- .reverseNamedList(popMapPaths)
          # }

          popPaths <- .reverseNamedList(cciaObj()$popPaths(
              popType(), includeFiltered = managerConf$flowPlot$showFilters,
              includeRoot = TRUE
          ))

          createGatingBoxPopSelect(
            local_boxIDs$i,
            popPaths,
            # unname(local_x()$getPlotPopPath()),
            names(local_x()$getPlotPopPath()),
            ignoreInput = TRUE
          )
        })

        # set leave selection
        output[[local_boxIDs$popLeaves]] <- renderUI({
          isolate({
            # prepare population leaves
            # popLeaves <- cciaObj()$popUtils(popType())$popLeaves(local_x()$getPlotPopPath())
            popLeaves <- cciaObj()$popLeaves(
              popType(), local_x()$getPlotPopPath(),
              includeFiltered = managerConf$flowPlot$showFilters)

            if (length(popLeaves) > 0) {
              popLeaves <- .reverseNamedList(popLeaves)
            }

            createGatingBoxPopLeaves(
              local_boxIDs$i, popLeaves,
              .reverseNamedList(local_x()$getPlotPopLeaves()),
              ignoreInput = TRUE
            )
          })
        })

        # set plot
        output[[local_boxIDs$plot]] <- renderPlotly({
          req(local_x()$getPlotPopPath())

          createGatingBoxPlot(local_x)
        })

        # set plot type
        output[[local_boxIDs$plotType]] <- renderUI({
          req(local_x()$getPlotType())

          createGatingBoxPlotType(
            local_boxIDs$i, local_x()$getPlotType(), ignoreInput = TRUE)
        })

        # show filters
        if (managerConf$flowPlot$showFilters == TRUE) {
          # set filter measures
          output[[local_boxIDs$filterMeasure]] <- renderUI({
            statsCols <- colnames(flowPopStats())

            # exclude label
            statsCols <- statsCols[statsCols != "label"]

            # add zero filtering
            statsCols <- c("NONE", statsCols)

            createGatingBoxFilterMeasures(
              local_boxIDs$i, statsCols, local_x()$getPlotFilterMeasure(),
              ignoreInput = TRUE
            )
          })

          # set filter values
          output[[local_boxIDs$filterValues]] <- renderUI({
            selectedMeasure <- input[[local_boxIDs$filterMeasure]]

            req(selectedMeasure)
            req(selectedMeasure != "NONE")

            statsValues <- unique(flowPopStats()[, ..selectedMeasure])

            createGatingBoxFilterValues(
              local_boxIDs$i, statsValues, local_x()$getPlotFilterValues(),
              ignoreInput = TRUE
            )
          })
        }
      })
    }
  })

  # set list of gating plots
  observeEvent(c(
    numFlowPlots(),
    # trigger when the selection changes
    moduleManagers()$imageViewerManager$imageSelected()
  ), {
    req(cciaObj())

    # check that there is a utility for that type
    req(cciaObj()$popUtils(popType()))

    # reset plots
    flowGatingPlotsShown(0)

    # create plots
    plots <- list()
    for (i in seq(numFlowPlots())) {
      # get input
      boxIDs <- flowGatingBoxIDs(i)

      # get saved plots
      savedPlots <- cciaObj()$imPopGatePlots(popType(), i)

      # set default plot params
      if (!is.null(savedPlots)) {
        popPath <- savedPlots$popPath
        popLeaves <- savedPlots$popLeaves
        filterMeasure <- savedPlots$filterMeasure
        filterValues <- savedPlots$filterValues
        x <- savedPlots$x
        y <- savedPlots$y
        xScale <- savedPlots$xScale
        yScale <- savedPlots$yScale
        plotType <- if (!is.null(savedPlots$plotType)) savedPlots$plotType else cciaConf()$fcs$gating$defaultPlotType
      } else {
        popPath <- shinyInputValue(
          boxIDs$popPath, input, "root",
          ignoreInput = initGatingBoxPlots())
        popLeaves <- shinyInputValue(
          boxIDs$popLeaves, input, NULL,
          ignoreInput = initGatingBoxPlots())
        filterMeasure <- shinyInputValue(
          boxIDs$filterMeasure, input, NULL,
          ignoreInput = initGatingBoxPlots())
        filterValues <- shinyInputValue(
          boxIDs$filterValues, input, NULL,
          ignoreInput = initGatingBoxPlots())
        x <- shinyInputValue(
          boxIDs$axisX, input, cciaObj()$imChannelNames()[[1]],
          ignoreInput = initGatingBoxPlots())
        y <- shinyInputValue(
          boxIDs$axisY, input, cciaObj()$imChannelNames()[[2]],
          ignoreInput = initGatingBoxPlots())
        xScale <- shinyInputValue(
          boxIDs$axisXScale, input, "linear",
          ignoreInput = initGatingBoxPlots())
        yScale <- shinyInputValue(
          boxIDs$axisYScale, input, "linear",
          ignoreInput = initGatingBoxPlots())
        plotType <- shinyInputValue(
          boxIDs$plotType, input, cciaConf()$fcs$gating$defaultPlotType,
          ignoreInput = initGatingBoxPlots())
      }
      
      # add names if root
      if (popPath == "root") names(popPath) <- "root"

      p1 <- FlowGatingPlot$new(boxIDs)$reactive()
      p1()$setPlotName(paste("Plot", i))
      p1()$setPlotPopPath(popPath)
      p1()$setPlotPopLeaves(popLeaves)
      p1()$setPlotFilterMeasure(filterMeasure)
      p1()$setPlotFilterValues(filterValues)
      p1()$setPlotXchannel(x)
      p1()$setPlotYchannel(y)
      p1()$setPlotXchannelScale(xScale)
      p1()$setPlotYchannelScale(yScale)
      p1()$setPlotType(plotType)

      plots[[i]] <- p1
    }

    # set plots
    flowGatingPlots(plots)

    # go through plots and create boxes
    counter <- 1
    for (x in flowGatingPlots()) {
      plots[[counter]] <- createGatingBox(x)

      counter <- counter + 1
    }

    # reset init flag
    initGatingBoxPlots(FALSE)

    # create new plot
    flowGatingBoxes(tagList(plots))
  })

  # listen to selected cells from viewer
  observeEvent(viewerSelectedLabels(), {
    req(flowGatingPlots())

    # go through plots
    for (x in flowGatingPlots()) {
      flowGatingPlotChangeTrace(x)
    }
  }, ignoreNULL = FALSE)

  # check that all plots have finished rendering
  observeEvent(flowGatingPlotsAfterplot(), {
    # do not react to further changes of the plots after initial plotting
    req(flowGatingPlotsShown() < numFlowPlots())

    # TODO Doesn't work
    flowGatingPlotsShown(
      flowGatingPlotsShown() + 1
    )
  })

  # trigger events that should run when all plots are plotted
  observeEvent(flowGatingPlotsAfterplot(), {
    req(flowGatingPlotsRendered())

    # reset afterplot
    flowAfterplot(TRUE)
  })

  # react to population selection
  observeEvent(
    c(
      flowGatingPlotsPopChanges()
      # flowGatingPlotsShown()
    ), {
      req(length(flowGatingPlots()) > 0)
      req(flowGatingPlotsRendered())

      # reset afterplot
      flowAfterplot(FALSE)

      # check which plot was changed
      for (x in flowGatingPlots()) {
        boxIDs <- x()$getBoxIDs()

        curInput <- input[[boxIDs$popPath]]
        curVal <- names(x()$getPlotPopPath())

        if (!is.null(curInput) && (is.null(curVal) || curInput != curVal)) {
          # set pop ID as attribute
          popPath <- unlist(cciaObj()$popAttr(
            popType(), "path", popIDs = curInput,
            includeFiltered = managerConf$flowPlot$showFilters
            ), use.names = FALSE)

          # was the population in the map?
          if (is.null(popPath)) {
            popPath <- curInput
          }

          names(popPath) <- curInput

          # reset pop
          x()$setPlotPopPath(popPath)

          # save to image
          flowSavePopGatePlot(x, invalidate = FALSE)

          # remove gates
          moduleManagers()$uiManager$plot_lyClearShapesWithAnnotations(boxIDs$plot)

          # reset leaves
          x()$setPlotPopLeaves(NULL)

          # get pop leaves
          popLeaves <- cciaObj()$popLeaves(
            popType(), popPath,
            includeFiltered = managerConf$flowPlot$showFilters
          )

          if (length(popLeaves) > 0) {
            popLeaves <- .reverseNamedList(popLeaves)
          }

          # update subpopulations
          updateSelectInput(
            session, boxIDs$popLeaves,
            # choices = cciaObj()$popUtils(popType())$popLeaves(curInput),
            choices = popLeaves,
            selected = NULL)

          # trigger gate redraw
          flowPopLeavesUpdated(runif(1))
        }
      }
    })

  # react to leave selection
  observeEvent(
    c(
      flowGatingPlotsLeaveChanges()
    ), {
      req(length(flowGatingPlots()) > 0)
      req(flowGatingPlotsRendered())
      # req(flowAfterplot())

      # reset afterplot
      flowAfterplot(FALSE)

      # check which plot was changed
      for (x in flowGatingPlots()) {
        boxIDs <- x()$getBoxIDs()

        plotInput <- flowPlotChanged(boxIDs$popLeaves, names(x()$getPlotPopLeaves()))
        if (!is.null(plotInput)) {
          # add leaves to existing leaves
          # rather than using the input, as these
          # will be synced with the traces on the plot
          curVal <- x()$getPlotPopLeaves()
          difLeaves <- plotInput$dif

          # traces to delete
          traceIDs <- c()

          # populations to plot
          popsToPlot <- c()

          # delete parent population?
          if (length(curVal) == 0) {
            traceIDs <- c(0)
            popsToPlot <- x()$getPlotPopPath()
          }

          if (difLeaves %in% names(curVal)) {
            traceIDs <- which(names(curVal) == difLeaves)
            curVal <- curVal[names(curVal) != difLeaves]
          } else {
            # get path for pop
            difPaths <- cciaObj()$popAttr(
              popType(), "path", popIDs = difLeaves,
              includeFiltered = managerConf$flowPlot$showFilters)

            # append to value
            curVal <- append(curVal, difPaths)

            # add to pops
            popsToPlot <- append(
              popsToPlot, difPaths)
          }

          # set leaves
          x()$setPlotPopLeaves(
            # cciaObj()$popAttr(popType(), "path", curVal),
            curVal, invalidate = FALSE)

          # save to image
          flowSavePopGatePlot(x, invalidate = FALSE)

          if (purrr::is_empty(curVal)) {
            # default traces if no subpopulations remain
            flowGatingPlotChangeTrace(
							x, removeTraceIDs = expandTraceIDs(c(0, 1), x()$getPlotType()))
          } else {
            # check that populations still exist
            # popsToPlot <- popsToPlot[cciaObj()$popUtils(popType())$popsInLeaves(
            #   popsToPlot, x()$getPlotPopPath(), includeParent = TRUE)]

            # redraw traces
            flowGatingPlotChangeTrace(
							x, ignoreNullPops = TRUE,
							removeTraceIDs = expandTraceIDs(traceIDs, x()$getPlotType()),
							popsToPlot = popsToPlot)
          }
        }
      }
    })

  # react to filter measure selection
  observeEvent(
    c(
      flowGatingPlotsFilterMeasureChanges()
    ), {
      req(length(flowGatingPlots()) > 0)
      req(flowGatingPlotsRendered())

      # check which plot was changed
      for (x in flowGatingPlots()) {
        boxIDs <- x()$getBoxIDs()

        plotInput <- flowPlotChanged(boxIDs$filterMeasure,
                                     x()$getPlotFilterMeasure())
        if (!is.null(plotInput)) {
          filterMeasure <- input[[boxIDs$filterMeasure]]

          if (plotInput$input == "NONE") {
            # reset filter
            x()$setPlotFilterMeasure("NONE", invalidate = FALSE)
            x()$setPlotFilterValues(NULL, invalidate = FALSE)
            x()$setPlotFilterLabels(NULL, invalidate = FALSE)

            # change traces
            flowGatingPlotChangeTrace(x, forceRedraw = TRUE,
                                      removeTraceIDs = NULL)

            # change viewer pops
            flowSavePops("root", purge = TRUE)

            # save to image
            flowSavePopGatePlot(x, invalidate = FALSE)
          }
        }
      }
    })

  # react to filter value selection
  observeEvent(
    c(
      flowGatingPlotsFilterValuesChanges()
    ), {
      req(length(flowGatingPlots()) > 0)
      req(flowGatingPlotsRendered())

      # check which plot was changed
      for (x in flowGatingPlots()) {
        boxIDs <- x()$getBoxIDs()

        plotInput <- flowPlotChanged(boxIDs$filterValues,
                                     x()$getPlotFilterValues())
        if (!is.null(plotInput)) {
          filterMeasure <- input[[boxIDs$filterMeasure]]
          filterValues <- plotInput$input

          # set filter
          if (filterMeasure %in% colnames(flowPopStats())) {
            labels <- flowPopStats()[get(filterMeasure) == filterValues, ]$label

            # add filtered labels to plot
            x()$setPlotFilterMeasure(filterMeasure, invalidate = FALSE)
            x()$setPlotFilterValues(filterValues, invalidate = FALSE)
            x()$setPlotFilterLabels(labels, invalidate = FALSE)

            # change traces
            flowGatingPlotChangeTrace(x, forceRedraw = TRUE,
                                      removeTraceIDs = NULL)

            # change viewer pops
            flowSavePops("root", purge = TRUE, labels = labels)

            # save to image
            flowSavePopGatePlot(x, invalidate = FALSE)
          }
        }
      }
    })

  # react to plot type selection
  observeEvent(
    c(
      flowGatingPlotsTypeChanges()
      # flowGatingPlotsShown()
    ), {
      req(length(flowGatingPlots()) > 0)
      req(flowGatingPlotsRendered())

      # reset afterplot
      flowAfterplot(FALSE)

      # check which plot was changed
      for (x in flowGatingPlots()) {
        boxIDs <- x()$getBoxIDs()
        curInputID <- boxIDs$plotType

        curInput <- input[[curInputID]]
        curVal <- x()$getPlotType()

        if (length(curVal) == 0 | curInput != curVal) {
          x()$setPlotType(curInput)

          # save to image
          flowSavePopGatePlot(x, invalidate = FALSE)

          # remove gates
          moduleManagers()$uiManager$plot_lyClearShapesWithAnnotations(boxIDs$plot)

          # trigger gate redraw
          flowPopLeavesUpdated(runif(1))
        }
      }
    })

  # react to channel selection
  observeEvent(
    c(
      flowGatingPlotsAxisChanges()
      # flowGatingPlotsShown()
    ), {
      req(length(flowGatingPlots()) > 0)
      req(flowGatingPlotsRendered())

      # reset afterplot
      flowAfterplot(FALSE)

      # check which plot was changed
      for (x in c("X", "Y")) {
        for (y in flowGatingPlots()) {
          boxIDs <- y()$getBoxIDs()
          curInputID <- boxIDs[[paste0("axis", x)]]

          curInput <- input[[curInputID]]
          if (x == "X") {
            curVal <- y()$getPlotXchannel()
          } else {
            curVal <- y()$getPlotYchannel()
          }

          if (curInput != curVal) {
            if (x == "X") {
              y()$setPlotXchannel(curInput)
            } else {
              y()$setPlotYchannel(curInput)
            }

            # save to image
            flowSavePopGatePlot(y, invalidate = FALSE)

            # remove gates
            moduleManagers()$uiManager$plot_lyClearShapesWithAnnotations(boxIDs$plot)

            # trigger gate redraw
            flowPopLeavesUpdated(runif(1))
          }
        }
      }
    })

  # react to channel scale selection
  observeEvent(
    c(
      flowGatingPlotsAxisScaleChanges()
    ), {
      req(length(flowGatingPlots()) > 0)
      req(flowGatingPlotsRendered())

      # reset afterplot
      flowAfterplot(FALSE)

      # check which plot was changed
      for (x in c("X", "Y")) {
        for (y in flowGatingPlots()) {
          boxIDs <- y()$getBoxIDs()
          curInputID <- boxIDs[[paste0("axis", x, "Scale")]]

          curInput <- input[[curInputID]]
          if (x == "X") {
            curVal <- y()$getPlotXchannelScale()
          } else {
            curVal <- y()$getPlotYchannelScale()
          }

          if (is.null(curVal) || curInput != curVal) {
            if (x == "X") {
              y()$setPlotXchannelScale(curInput)
            } else {
              y()$setPlotYchannelScale(curInput)
            }

            # save to image
            flowSavePopGatePlot(y, invalidate = FALSE)

            # trigger redraw
            moduleManagers()$uiManager$plot_lyClearShapesWithAnnotations(boxIDs$plot)

            # redraw
            # flowGatingPlotChangeTrace(y, removeTraceIDs = NULL)
          }
        }
      }
    })

  # changed colour for population
  observeEvent(moduleManagers()$populationManager$changedColourForPop(), {
    req(moduleManagers()$populationManager$changedColourForPop())

    popInfo <- moduleManagers()$populationManager$changedColourForPop()

    # toggle traces in plots
    for (x in popInfo) {
      for (y in flowGatingPlots()) {
        yIDs <- y()$getBoxIDs()

        # update traces if the population is shown as subpopulation
        if (x$path %in% y()$getPlotPopLeaves()) {
          # update plot leaves
          flowUpdatePopLeaves(y, x$path)
        }
      }
    }
  })

  # show selected cells on image
  observeEvent(flowGatingPlotsSelected(), {
    req(length(flowGatingPlotsSelected()) > 0)
    req(globalManagers$viewerManager()$viewer())

    # get selected labels
    # TODO does this work for multiple plots .. ?
    customdata <- flowGatingPlotsSelected()[[1]]$customdata

    # group value names and labels
    # https://stackoverflow.com/a/44760488/13766165
    labelsGroup <- data.table(customdata = customdata) %>%
      separate(customdata, c("value_name", "label"), sep = "#")

    # select cells on image
    for (x in unique(labelsGroup$value_name)) {
      globalManagers$viewerManager()$viewer()$highlightLabels(
        x, labelsGroup[value_name == x]$label
      )
    }
  })

  # reset cell selection from image
  observeEvent(input$resetImageLabelSelection, {
    req(input$resetImageLabelSelection)

    viewerSelectedLabels(NULL)
  })

  ## Generic

  ### UI Outputs
  ## Tables

  ## Plots
  output$flowPlots <- renderUI({
    req(flowGatingBoxes())

    flowGatingBoxes()
  })

  ## Buttons

  ## Other

  ## public functions
  list(
    flowGatingPlotsSelected = flowGatingPlotsSelected,
    flowGatingPlotsRendered = flowGatingPlotsRendered,
    flowGatingPlotsRelayout = flowGatingPlotsRelayout,
    initGatingBoxPlots = initGatingBoxPlots,
    flowSavePops = flowSavePops,
    flowUnlinkPops = flowUnlinkPops,
    flowUpdatePopLeaves = flowUpdatePopLeaves,
    flowPopLeavesUpdated = flowPopLeavesUpdated,
    flowAfterplot = flowAfterplot,
    flowGatingPlots = flowGatingPlots,
    flowGatingBoxIDs = flowGatingBoxIDs,
    viewerSelectedLabels = viewerSelectedLabels,
    flowSavePopGatePlot = flowSavePopGatePlot,
    flowGatingPlotChangeTrace = flowGatingPlotChangeTrace,
    debugPlotsRendered = debugPlotsRendered
  )
}
