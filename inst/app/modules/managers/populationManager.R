# manage populations from gating or mapping
createPopulationManager <- function(
  input, output, session, globalManagers, moduleManagers, managerConf) {
  # set default parameters
  if (!"enableAddPopulation" %in% names(managerConf$population)) {
    managerConf$population$enableAddPopulation <- FALSE
  }
  if (!"enableEditPopName" %in% names(managerConf$population)) {
    managerConf$population$enableEditPopName <- FALSE
  }
  if (!"enableFilterPopulation" %in% names(managerConf$population)) {
    managerConf$population$enableFilterPopulation <- FALSE
  }
  if (!"popsFilteredFromValueName" %in% names(managerConf$population)) {
    managerConf$population$popsFilteredFromValueName <- FALSE
  }
  
  colourChoice <- function(popID = NULL, failed = FALSE) {
    colourChoices <- cciaConf()$colours$predefined
    
    createActionButton <- function(popID, colID, colour, selected = FALSE) {
      actionButton(
        session$ns(sprintf("colourChoice_%d_%d", popID, colID)), NULL,
        icon = if (selected == TRUE)
          icon(btnICON_SELECTED)
        else
          icon(btnICON_NOTSELECTED),
        style = sprintf("background-color: %s;", colour),
        onclick = sprintf(
          'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
          session$ns("colourChoice"),
          paste(popID, colour, sep = ".")
        )
      )
    }
    
    # create buttons for colour choices
    uiChoices <- list()
    if (!is.null(popID)) {
      counter <- 1
      for (curColour in colourChoices) {
        uiChoices[[counter]] <- createActionButton(
          popID, counter, curColour)
        
        counter <- counter + 1
      }
    } else {
      uiButtons <- list()
      
      popCounter <- 1
      # build choice for all populations
      for (curPop in names(popData())) {
        x <- popData()[[curPop]]
        
        # get selected colour
        selectedCol <- popData()[[as.numeric(popCounter)]]$colour
        
        colCounter <- 1
        for (curColour in colourChoices) {
          uiButtons[[colCounter]] <- createActionButton(
            popCounter, colCounter, curColour,
            if (selectedCol == curColour) TRUE else FALSE)
          
          colCounter <- colCounter + 1
        }
        
        # get short parent path
        parentPath <- .flowTrimPath(x$parent)
        
        # put into row
        uiChoices[[popCounter]] <- fluidRow(
          column(3, tags$label(
            paste(parentPath, x$name, sep = "/")
            )),
          column(9, uiButtons)
        )
        
        popCounter <- popCounter + 1
      }
    }
    
    # build the modal
    modalDialog(
      tagList(uiChoices),
      easyClose = TRUE,
      size = 'l'
    )
  }
  
  # create population data
  createPopData <- function(attrToGet = c("filterMeasure", "filterMeasures",
                                          "filterFun", "filterFuns", "filterValues")) {
    # get object information
    # filter for relevant data
    popMap <- cciaObj()$imPopMap(modulePopType(), includeFiltered = TRUE)
    popInfo <- lapply(popMap, function(x) {
      x <- x[names(x) %in% attrToGet]
      
      # rename filter measures and funs to display them
      # in the same columns are their single value parts
      names(x)[names(x) == "filterMeasures"] <- "filterMeasure"
      names(x)[names(x) == "filterFuns"] <- "filterFun"
      
      x
    })
    
    # create value columns
    datCols <- list()
    
    for (i in attrToGet) {
      datCols[[i]] <- lapply(popInfo, function(x)
        if (!is.null(x[[i]])) {
          if (length(x[[i]]) > 0)
            paste(x[[i]], collapse = ", ")
          else
            x[[i]]
        } else ""
      )
    }
    
    # bind to dataframe
    do.call(cbind, datCols)
  }
  
  ### Reactive values
  popTable <- reactiveVal()
  popTableColumns <- reactiveVal()
  
  # values for modules to listen on
  deletedPops <- reactiveVal()
  renamedPops <- reactiveVal()
  addedPops <- reactiveVal()
  changedColourForPop <- reactiveVal()
  updatePopTable <- reactiveVal()
  
  # pop map variables
  showPopMapInteractive <- reactiveVal()
  
  ### Reactive-like values
  
  ### Reactives - RxCalc
  ## Event specific
  
  ## Generic
  # ccia Object
  cciaObj <- reactive({
    managerConf$cciaObj()
  })
  
  # get population data
  popData <- reactive({
    req(managerConf$cciaObj())
    
    if ("imPopMap" %in% names(managerConf$population))
      managerConf$population$imPopMap()
    else
      managerConf$cciaObj()$imPopMap(
        modulePopType(),
        includeFiltered = managerConf$population$enableFilterPopulation
      )
  })
  
  # population data from module
  modulePopData <- reactive({
    managerConf$population$popData()
  })
  
  # what kind of population definition is the module using?
  modulePopType <- reactive({
    managerConf$population$popType()
  })
  
  # which populations are defined?
  modulePopPaths <- reactive({
    req(cciaObj())
    req(modulePopType())
    req(cciaObj()$popUtils(modulePopType()))
    
    # get all pops
    # cciaObj()$popUtils(modulePopType())$popPaths()
    unname(cciaObj()$popPaths(modulePopType(), includeFiltered = TRUE))
  })
  
  # should the image be updated automatically?
  autoUpdateImage <- reactive({
    updateImage <- FALSE
    
    if (input$autoUpdateImage == TRUE &&
        globalManagers$viewerManager()$getShowViewer() == TRUE &&
        # check that the current is selected
        input$sidebar == session$ns(c())) {
      updateImage <- TRUE
    }

    updateImage
    
    # TRUE
  })
  
  # population stats
  flowPopStats <- eventReactive(c(
    moduleManagers()$imageViewerManager$imageSelected(),
    modulePopType()
  ), {
    req(cciaObj())
    
    cciaObj()$popStats(modulePopType())
  })
  
  # number of filters
  filteredPopNumMeasures <- reactive({
    input$filteredPopNumMeasures
  })
  
  # ids for filters
  filteredPopNumMeasures <- reactive({
    input$filteredPopNumMeasures
  })
  
  # filter measures
  filteredPopMeasures <- reactive({
    req(filteredPopNumMeasures())
    
    observeInputs(
      sprintf("filteredPopMeasure_%d", seq(filteredPopNumMeasures())),
      input, includeNULL = FALSE, inlcudeEmpty = FALSE)
  })
  
  # filter measure types
  filteredPopMeasureTypes <- reactive({
    req(filteredPopMeasures())
    
    lapply(filteredPopMeasures(), .cciaStatsType)
  })
  
  # filter funs
  filteredPopFuns <- reactive({
    req(filteredPopNumMeasures())
    
    observeInputs(
      sprintf("filteredPopFun_%d", seq(filteredPopNumMeasures())),
      input, includeNULL = FALSE, inlcudeEmpty = FALSE)
  })
  
  # filter values
  filteredPopValues <- reactive({
    req(filteredPopNumMeasures())
    
    observeInputs(
      sprintf("filteredPopValues_%d", seq(filteredPopNumMeasures())),
      input, includeNULL = FALSE, inlcudeEmpty = FALSE)
  })
  
  ### Observers - RxAction
  ## Event specific
  
  # propagate population mapping to whole set
  observeEvent(input$propagatePopMapping, {
    req(cciaObj())
    req(length(moduleManagers()$selectionManager$selectedUIDs()) > 0)
    req(moduleManagers()$imageSetManager$selectedSet())
    
    progress <- Progress$new()
    
    progress$set(
      message = "Propagate population map",
      value = 50)
    
    # copy to selected
    moduleManagers()$imageSetManager$selectedSet()$propagatePopMap(
      modulePopType(),
      fromUID = cciaObj()$getUID(),
      toUIDs = moduleManagers()$selectionManager$selectedUIDs()
    )
    
    # copy gating
    if (modulePopType() == "flow") {
      moduleManagers()$imageSetManager$selectedSet()$propagateFlowGating(
        fromUID = cciaObj()$getUID(),
        toUIDs = moduleManagers()$selectionManager$selectedUIDs(),
        recompute = TRUE
      )
    }
    
    progress$close()
  })
  
  # save mapping if not done during processing
  observeEvent(input$savePopMapping, {
    req(cciaObj())
    req(DEBUG_SHOW_VIEWER == TRUE)
    req(globalManagers$projectManager()$getProjectType() != "flow")
    
    # save pop map
    cciaObj()$savePopMap(
      modulePopType(),
      includeFiltered = managerConf$population$enableFilterPopulation
      )
    
    # save populations
    cciaObj()$savePops(
      modulePopType(),
      includeFiltered = managerConf$population$enableFilterPopulation,
      flushCache = TRUE
      )
  })
  
  # show dialog to change colour for population
  observeEvent(input$changeColourForPop, {
    # showModal(colourChoice(input$changeColourForPop))
    # show selection for all
    showModal(colourChoice())
  })
  
  # change colour for population
  observeEvent(input$colourChoice, {
    params = stringr::str_split(input$colourChoice, pattern = "\\.")[[1]]
    
    popID <- params[[1]]
    colour <- params[[2]]
    
    # get pop name
    popName <- names(popData())[[as.numeric(popID)]]
    
    # change colour
    cciaObj()$editPopColour(
      modulePopType(), popName, colour,
      includeFiltered = managerConf$population$enableFilterPopulation
    )
    
    if (DEBUG_SHOW_VIEWER == TRUE && globalManagers$projectManager()$getProjectType() != "flow") {
      # save pop map
      cciaObj()$savePopMap(
        modulePopType(),
        includeFiltered = managerConf$population$enableFilterPopulation)
    }
    
    # toggle colour selection for population
    counter <- 1
    for (curColour in cciaConf()$colours$predefined) {
      labelID <- sprintf(
        "colourChoice_%s_%s", popID, counter)
      
      if (colour == curColour) {
        html(labelID, htmlIcon(btnICON_SELECTED, btnClass = ""))
      } else {
        html(labelID, htmlIcon(btnICON_NOTSELECTED, btnClass = ""))
      }
      
      counter <- counter + 1
    }
    
    # get population info
    popInfo <- cciaObj()$imPopMap(
      modulePopType(), popName
    )
    
    # set changed colour
    changedColourForPop(popInfo)
  })
  
  # update image
  observeEvent(c(
    # input$updateImage,
    managerConf$population$updateImage(),
    moduleManagers()$imageViewerManager$imageShown()
    ), {
    req(cciaObj())
    req(modulePopType())
    req(globalManagers$viewerManager()$viewer())
    
    # do not show if the image has not been loaded yet
    req(moduleManagers()$imageViewerManager$imageShown())
    
    # was the image just selected?
    execInteractive <- TRUE
    
    if (!is.null(showPopMapInteractive())) {
      if (showPopMapInteractive() == moduleManagers()$imageViewerManager$imageShown()) {
        execInteractive <- TRUE
        
        # reset interactive pop map
        showPopMapInteractive(moduleManagers()$imageViewerManager$imageShown())
      }
    }
    
    # show mapping
    if (globalManagers$viewerManager()$getShowPops() == TRUE) {
      # TODO go through pop types
      globalManagers$viewerManager()$viewer()$showPopMapping(
        modulePopType(),
        filteredFromValueName = managerConf$population$popsFilteredFromValueName,
        pointsSize = input$popPointsSize,
        execInteractive = execInteractive
      )
    }
    
    # show neighbours
    if (globalManagers$viewerManager()$getShowNeighbours() == TRUE) {
      if (length(cciaObj()$imNeighboursFilepath()) > 0) {
        globalManagers$viewerManager()$viewer()$showCellNeighbours(
          modulePopType(),
          execInteractive = execInteractive
        )
      }
    }
  })
  
  # listen to the population table
  # https://www.r-bloggers.com/2019/04/edit-datatables-in-r-shiny-app/
  observeEvent(input$popTable_cell_edit, {
    # get events from edit
    editEvents = input$popTable_cell_edit
    
    # get cell coordinates in table
    tblRow <- editEvents$row
    
    # TODO does this start at 0 .. ?
    tblCol <- editEvents$col + 1
    tblValue <- editEvents$value
    
    # get pop ID
    popID <- names(popData())[[as.numeric(tblRow)]]
    
    # get info
    popPrevInfo <- cciaObj()$imPopMap(
      modulePopType(), popID
    )
    
    # rename
    if (managerConf$population$enableEditPopName == FALSE) {
      # reset information
      updatePopTable(runif(1))
    } else {
      if (tblCol == 5) {
        # rename in object
        cciaObj()$editPopName(
          modulePopType(), popID, tblValue,
          includeFiltered = managerConf$population$enableFilterPopulation
        )
        
        # get population info
        popInfo <- cciaObj()$imPopMap(
          modulePopType(), popID
        )
        
        # put in old name
        popInfo[[1]]$prevName <- popPrevInfo[[1]]$name
        
        # put in random number to trigger reactives
        popInfo[[1]]$rID <- runif(1)
        
        # set renamed populations
        renamedPops(popInfo)
      }
    }
  })
  
  # change point sizes
  observeEvent(c(
    input$popPointsSize,
    moduleManagers()$imageViewerManager$imageShown()
  ), {
    req(cciaObj())
    req(modulePopType())
    req(globalManagers$viewerManager()$viewer())
    
    # do not show if the image has not been loaded yet
    req(moduleManagers()$imageViewerManager$imageShown())
    
    # call viewer
    globalManagers$viewerManager()$viewer()$setPopPointsSize(
      modulePopType(), input$popPointsSize, execInteractive = FALSE
    )
  })
  
  # create population
  observeEvent(input$addPop, {
    req(input$popName)
    req(input$popParent)
    req(cciaObj())
    
    popID <- cciaObj()$addPop(
      modulePopType(), input$popName,
      includeFiltered = managerConf$population$enableFilterPopulation,
      popAttr = list(
        parent = input$popParent,
        # by default set to parent/name
        path = paste(
          .flowNormRootPath(input$popParent),
          input$popName, sep = "/"
        )
      )
    )
    
    popInfo <- cciaObj()$imPopMap(
      modulePopType(), popID
    )
    
    # set added populations
    addedPops(popInfo)
  })
  
  # add filtered populations to pop map
  observeEvent(input$addFilteredPop, {
    req(input$filteredPopName)
    req(input$filteredPopParents)
    req(filteredPopMeasures())
    req(filteredPopFuns())
    req(filteredPopValues())
    
    # return pop info
    popInfo <- list()
    
    # convert to numeric
    filterValues <- mapply(
      function(x, i, y) {
        if (x == "numeric")
          as.double(y)
        else if (x == "logical")
          as.logical(y)
        else
          y
      },
      filteredPopMeasureTypes(), names(filteredPopMeasureTypes()),
      filteredPopValues(), SIMPLIFY = FALSE
    )
    
    # define pop
    pops <- list()
    pops[[input$filteredPopName]] <- list(
      filterMeasures = unname(filteredPopMeasures()),
      filterValues = unname(filterValues),
      filterFuns = unname(filteredPopFuns()),
      filterDefaultAll = input$filterDefaultAll,
      isTrack = input$isTrack
    )
    
    # go through parents and add populations
    # TODO the value name can change between parent pops
    # otherwise you could add them all in one call?
    popIDs <- c()
    for (x in input$filteredPopParents) {
      # get parent ID
      parentID <- cciaObj()$popIDsByAttr(
        modulePopType(), "path", x, includeFiltered = TRUE)
      
      # get value name from parent
      valueName <- cciaObj()$popAttr(
        modulePopType(), popAttr = "valueName",
        popIDs = c(parentID), includeFiltered = TRUE)[[1]]
      
      # remove previous populations
      cciaObj()$delPopsByPath(
        modulePopType(),
        pops = levels(interaction(x, names(pops), sep = "/")),
        includeFiltered = TRUE
      )
      
      # add populations
      popIDs <- c(
        popIDs,
        cciaObj()$addFilteredPops(modulePopType(), x, pops,
                                valueName = valueName)
      )
    }
    
    # set added populations
    addedPops(cciaObj()$imPopMap(modulePopType(), popIDs))
  })
  
  # delete population
  observeEvent(input$delPop, {
    req(cciaObj())
    
    # get population info
    popInfo <- cciaObj()$imPopMap(
      modulePopType(), input$delPop
    )
    
    # delete population from image
    cciaObj()$delPop(
      modulePopType(), input$delPop,
      includeFiltered = managerConf$population$enableFilterPopulation
    )
    
    # set deleted populations
    deletedPops(popInfo)
  })
  
  # toggle population visibility
  observeEvent(input$toggleVisibilityForPop, {
    req(cciaObj())
    req(globalManagers$viewerManager()$viewer())
    
    popID <- input$toggleVisibilityForPop
    
    popName <- popData()[[popID]]$name
    
    # toggle in object
    cciaObj()$toggleVisibilityForPop(
      modulePopType(), popID, invalidate = FALSE,
      includeFiltered = managerConf$population$enableFilterPopulation
    )
    
    if (DEBUG_SHOW_VIEWER == TRUE && globalManagers$projectManager()$getProjectType() != "flow") {
      # save pop map
      cciaObj()$savePopMap(
        modulePopType(),
        includeFiltered = managerConf$population$enableFilterPopulation)
    }
    
    # toggle visibility on viewer and table
    labelID <- sprintf(
      "toggleVisibilityForPop_%s", popID)
    
    popLayerName <- globalManagers$viewerManager()$viewer()$popLayerName(
      modulePopType(), popName
    )
    
    # show population?
    popShow <- cciaObj()$popAttr(
      modulePopType(), "show", popID,
      includeFiltered = managerConf$population$enableFilterPopulation)
    
    if (popShow == TRUE) {
      globalManagers$viewerManager()$viewer()$showLayer(popLayerName)
      html(labelID, htmlIcon(btnICON_SHOWN))
    } else {
      globalManagers$viewerManager()$viewer()$hideLayer(popLayerName)
      html(labelID, htmlIcon(btnICON_HIDDEN))
    }
  })
  
  # generate population table
  observeEvent(c(
    popData(), modulePopData()
  ), {
    req(cciaObj())
    
    # reset pop table if data empty but the
    # user added populations before
    if (is.null(popData()) && !is.null(popTableColumns())) {
      popTable(popTableColumns())
    }
    
    req(popData())
    
    if (length(names(popData())) > 0) {
      # fill defined populations
      counter <- 1
      popColours <- c()
      popVisibility <- c()
      for (curPop in popData()) {
        # add attributes
        popColours <- c(
          popColours, curPop$colour
        )
        popVisibility <- c(
          popVisibility, curPop$show
        )
        
        counter <- counter + 1
      }
      
      # prepare populations
      popIDs <- names(popData())
      nPops <- length(popIDs)
      
      popCols <- list()
      
      # create visibility icons
      visibilityIcons <- rep(btnICON_SHOWN, length(popVisibility))
      visibilityIcons[popVisibility == FALSE] <- btnICON_HIDDEN
      
      # create colour and delete column
      modCols <- list(
        " " = shinyInput(
          "actionLink", session$ns("delPop_"), popIDs,
          initIcons = rep(btnICON_DELETE, nPops),
          initClasses = rep(labelCLASS_IMPORTANT, nPops),
          initOnclick = paste(
            sprintf(
              'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
              session$ns("delPop"),
              popIDs
            )
          )
        ),
        " " = shinyInput(
          "actionLink", session$ns("toggleVisibilityForPop_"), popIDs,
          initIcons = visibilityIcons,
          initOnclick = paste(
            sprintf(
              'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
              session$ns("toggleVisibilityForPop"),
              popIDs
            )
          )
        ),
        " " = shinyInput(
          "actionButton", session$ns("colourPop_"), popIDs,
          initStyles = paste(sprintf("background-color: %s;", popColours)),
          initOnclick = paste(
            sprintf(
              'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
              session$ns("changeColourForPop"),
              popIDs
            )
          )
        ),
        parent = unlist(
          lapply(popData(), function(x) x$parent), use.names = FALSE),
        name = unlist(
          lapply(popData(), function(x) x$name), use.names = FALSE)
      )
      
      # add to list
      popCols <- append(
        popCols, modCols)
      
      # create dataframe
      popTableDF <- do.call(cbind, popCols)
      
      # add columns specific to module
      popTableDF <- cbind(popTableDF, modulePopData())
      
      # add names
      # colnames(popTableDF) <- names(popCols)
      # rownames(popTableDF) <- rownames(popCols)
    } else {
      popTableDF <- NULL
    }
    
    # set pop table columns
    if (!is.null(popTableDF)) {
      if (is.null(popTableColumns()) || !any(colnames(popTableColumns()) %in% colnames(popTableDF))) {
        cols <- as.data.frame(
          rep(list(""), length(colnames(popTableDF))))
        colnames(cols) <- colnames(popTableDF)
        
        popTableColumns(cols)
      }
    } else {
      popTableColumns(NULL)
    }
    
    # set pop table
    popTable(popTableDF)
  }, ignoreNULL = FALSE)
  
  ## Generic
  
  ### UI Outputs
  ## Tables
  # Populations
  output$popTable <- DT::renderDataTable({
    req(popTableColumns())
    
    # get table
    options = list(
      fixedColumns = list(leftColumns = 1),
      fixedHeader = TRUE)
    
    tableOpts <- list(
      ordering = TRUE,
      dom = "tip",
      pageLength = 10
    )
    # get pop table options
    if ("popTable" %in%  names(managerConf$population)) {
      tableOpts <- optsFromList(
        managerConf$population$popTable, tableOpts
      )
    }
    
    moduleManagers()$uiManager$dataTable(
      popTableColumns(), options = options, rownames = TRUE, editable = TRUE,
      ordering = tableOpts$ordering, pageLength = tableOpts$pageLength, dom = tableOpts$dom)
  })
  
  # update table without triggering redraw
  observeEvent(c(
    popTable(), updatePopTable()
    ), {
    req(popTable())
    
    # https://stackoverflow.com/a/56879871/13766165
    replaceData(dataTableProxy("popTable"),
                popTable(),
                resetPaging = FALSE, rownames = FALSE)
  })
  
  ## Plots
  
  ## Buttons
  
  ## Other
  # adjust point sizes for population
  output$popPointsSize <- renderUI({
    # create slider for pop sizes
    fluidRow(
      column(3, tags$label("Points size")),
      column(
        9, sliderInput(
          session$ns("popPointsSize"), NULL,
          min = 1, max = 100,
          value = 6, step = 1
        )
      )
    )
  })
  
  # show populations to add pop
  output$popParent <- renderUI({
    req(modulePopPaths())
    
    # show select input with populations from pop type
    selectInput(session$ns("popParent"), "Parent",
                choices = append(
                  list(root = "root"),
                  modulePopPaths()
                ), multiple = FALSE,
                selected = shinyInputValue("popParent", input))
  })
  
  # show populations for filtering
  output$filteredPopParents <- renderUI({
    req(managerConf$population$enableFilterPopulation)
    req(modulePopPaths())
    
    # show select input with populations from pop type
    selectInput(session$ns("filteredPopParents"), "Parents",
                choices = modulePopPaths(), multiple = TRUE,
                selected = shinyInputValue("filteredPopParents", input))
  })
  
  # show measures for filtering
  output$filteredPopMeasure <- renderUI({
    req(managerConf$population$enableFilterPopulation)
    req(flowPopStats())
    req(filteredPopNumMeasures())
    
    filterMeasures <- colnames(flowPopStats())
    filterMeasures <- filterMeasures[
      !filterMeasures %in% c("label", "valueName")]
    
    # show select input with measures
    tagList(
      lapply(
        seq(filteredPopNumMeasures()),
        function(x) {
          inputID <- sprintf("filteredPopMeasure_%d", x)
          
          selectInput(session$ns(inputID), NULL,
                      choices = filterMeasures, multiple = FALSE,
                      selected = shinyInputValue(inputID, input))
        }
      )
    )
  })
  
  # show funs for filtering
  output$filteredPopFun <- renderUI({
    req(managerConf$population$enableFilterPopulation)
    req(flowPopStats())
    
    # show select input with functions
    tagList(
      lapply(
        seq(filteredPopNumMeasures()),
        function(x) {
          inputID <- sprintf("filteredPopFun_%d", x)
          
          selectInput(session$ns(inputID), NULL,
                      choices = .reverseNamedList(cciaConf()$parameters$filterFuns),
                      multiple = FALSE,
                      selected = shinyInputValue(inputID, input))
        }
      )
    )
  })
  
  # show values for filtering
  output$filteredPopValues <- renderUI({
    req(managerConf$population$enableFilterPopulation)
    req(length(filteredPopMeasureTypes()) > 0)
    
    # show input for values
    isolate({
      tagList(
        mapply(
          function(x, i) {
            inputID <- sprintf("filteredPopValues_%d", x)
            filterMeasure <- filteredPopMeasures()[[i]]
            filterType <- filteredPopMeasureTypes()[[i]]
            
            # check type of measure
            if (filterType %in% c("logical", "categorical")) {
              statsValues <- unique(flowPopStats()[, ..filterMeasure])
              
              # show select input with populations from pop type
              selectInput(session$ns(inputID), NULL,
                          choices = statsValues, multiple = TRUE,
                          selected = shinyInputValue(inputID, input))
            } else {
              textInput(session$ns(inputID), NULL,
                        value = shinyInputValue(inputID, input))
            }
          }, seq(filteredPopNumMeasures()), names(filteredPopMeasures()),
          SIMPLIFY = FALSE
        )
      )
    })
  })
  
  ## public functions
  list(
    autoUpdateImage = autoUpdateImage,
    deletedPops = deletedPops,
    renamedPops = renamedPops,
    addedPops = addedPops,
    changedColourForPop = changedColourForPop,
    createPopData = createPopData,
    flowPopStats = flowPopStats
  )
}
