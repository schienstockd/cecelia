# Server to create new project
createImageSetManager <- function(
  input, output, session, globalManagers, moduleManagers, managerConf) {
  # set default parameters
  if (!"enableAddition" %in% names(managerConf$imageSet)) {
    managerConf$imageSet$enableAddition <- FALSE
  }
  if (!"enableDeletion" %in% names(managerConf$imageSet)) {
    managerConf$imageSet$enableDeletion <- FALSE
  }
  
  ### Reactive values
  imageSetFiltersSelected <- reactiveVal()
  
  ### Reactive-like values
  
  ### Reactives - RxCalc
  ## Event specific
  
  ## Generic
  # get selected set
  selectedSet <- reactive({
    req(input$selectSet)
    req(globalManagers$dataManager()$cciaImageCollection())
    
    curSelected <- NULL
    if (length(globalManagers$dataManager()$cciaImageCollection()$cciaObjects()) > 0) {
      curSelected <- globalManagers$dataManager()$cciaImageCollection()$cciaObjectByUID(input$selectSet)
      if (length(curSelected) > 0) {
        curSelected <- curSelected[[1]]()
      }
    }
    
    curSelected
  })
  
  # available image data sets
  imageSets <- reactive({
    req(globalManagers$dataManager()$cciaImageCollection())
    
    objNames <- globalManagers$dataManager()$cciaImageCollection()$cciaObjectNames()
    
    req(length(objNames) > 0)
    
    # switch around
    setNames <- names(objNames) 
    names(setNames) <- sprintf("%s (%s)", objNames, names(objNames))
    
    if (is.null(setNames)) {
      setNames <- list()
    }
    
    setNames
  })
  
  # images set attributes
  imageSetAttributes <- reactive({
    req(selectedSet())
    req(length(selectedSet()) > 0)
    
    selectedSet()$summary(c("Attr"), withSelf = FALSE)
  })
  
  # invert attribute filter?
  imageSetFiltersInvert <- reactive({
    input$attrFilterInvert
  })
  
  # images set filters
  imageSetFilters <- reactive({
    req(imageSetFilterIDs())
    
    observeInputs(imageSetFilterIDs(), input)
  })
  
  # images set filter values
  imageSetFilterValues <- reactive({
    req(imageSetFilterIDs())
    
    filterValues <- mapply(
      function(x, i) {
        setAttrs <- unique(imageSetAttributes()[[i]])
        setAttrs[!is.na(setAttrs)]
      }, imageSetFilterIDs(), names(imageSetFilterIDs()),
      SIMPLIFY = FALSE
    )
    
    filterValues
  })
  
  # return filtered uIDs
  filteredUIDs <- reactive({
    req(imageSetAttributes())
    
    uIDs <- imageSetAttributes()$uID
    applyFilters <- imageSetFiltersSelected()[lengths(imageSetFiltersSelected()) > 0]
    
    # is any thing set?
    if (length(applyFilters) > 0) {
      remainingAttrs <- imageSetAttributes()
      
      # apply filters 
      for (i in names(applyFilters)) {
        # get attribute to filter for
        filterCol <- stringr::str_replace(i, "attrFilter_[:alnum:]+_", "")
        
        # apply
        if (imageSetFiltersInvert() == TRUE) {
          remainingAttrs <- remainingAttrs %>%
            dplyr::filter(!get(filterCol) %in% applyFilters[[i]])
        } else {
          remainingAttrs <- remainingAttrs %>%
            dplyr::filter(get(filterCol) %in% applyFilters[[i]])
        }
      }
      
      # set uIDs
      uIDs <- remainingAttrs$uID
    }
    
    uIDs
  })
  
  # filter input ids
  imageSetFilterIDs <- reactive({
    req(imageSetAttributes())
    
    # create lists
    filterIDs <- names(imageSetAttributes())
    names(filterIDs) <- filterIDs
    
    lapply(filterIDs, function(x) sprintf("attrFilter_%s_%s", selectedSet()$getUID(), x))
  })
  
  ### Observers - RxAction
  ## Event specific
  
  # listen to selected set
  observeEvent(imageSets(), {
    # toggle selection
    if (length(imageSets()) > 0) {
      enable("selectSet")
      enable("reloadSet")
    } else {
      disable("selectSet")
      disable("reloadSet")
    }
  })
  
  # listen to filters
  observeEvent(imageSetFilters(), {
    # toggle selection
    if (any(lengths(imageSetFilters()) > 0)) {
      enable("attrFilterApply")
      enable("attrFilterReset")
      enable("attrFilterInvert")
    } else {
      disable("attrFilterApply")
      disable("attrFilterReset")
      disable("attrFilterInvert")
    }
  })
  
  # listen to filter apply
  observeEvent(input$attrFilterApply, {
    req(imageSetFilters())
    
    # set filters
    imageSetFiltersSelected(imageSetFilters())
  })
  
  # listen to filter reset
  observeEvent(input$attrFilterReset, {
    req(imageSetFilterIDs())
    req(imageSetAttributes)
    
    # update selection inputs
    for (i in names(imageSetFilterIDs())) {
      updateSelectInput(
        session, imageSetFilterIDs()[[i]],
        choices = imageSetFilterValues()[[i]],
        selected = NULL)
    }
    
    # set filters
    imageSetFiltersSelected(NULL)
  })
  
  # reload set
  observeEvent(input$reloadSet, {
    req(selectedSet())
    
    # retrieve state of current set
    selectedSet()$retrieveState(
      includeChildren = TRUE, forceDataReload = FALSE)
  })
  
  # reset set
  observeEvent(input$resetSet, {
    req(selectedSet())
    
    # retrieve state of current set
    selectedSet()$retrieveState(
      includeChildren = TRUE, reset = TRUE)
  })
  
  # load data for set
  observeEvent(input$forceDataReloadForSet, {
    req(selectedSet())
    
    # retrieve state of current set
    selectedSet()$retrieveState(
      includeChildren = TRUE, forceDataReload = TRUE)
  })
  
  # delete image set
  # https://stackoverflow.com/a/52909678
  observeEvent(input$deleteSet, {
    req(selectedSet())
    
    showModal(modalDialog(
      tagList(h3("Are you sure?")), 
      title = "Delete Set",
      footer = tagList(actionButton(session$ns("deleteSetConfirmed"), "Delete set"),
                       modalButton("Cancel")
      )
    ))
  })
  
  # observeEvent(input$deleteSet, {
  observeEvent(input$deleteSetConfirmed, {
    req(selectedSet())
    
    # remove content from HPC
    if (globalManagers$projectManager()$useHPC() == TRUE) {
      progress <- Progress$new()
      
      progress$set(message = "Delete HPC files ... ", value = 50)
      
      for (curObj in selectedSet()$cciaObjects()) {
        globalManagers$projectManager()$deleteHPCpersistentObjectDirectory(
          curObj()$getUID()
        )
      }
      
      progress$close()
    }
    
    # remove set and its child objects
    selectedSet()$removeCciaObjects()
    selectedSet()$deleteObjectDirectory()
    globalManagers$dataManager()$cciaImageCollection()$removeCciaObjectByUID(selectedSet()$getUID())
    
    removeModal()
  })
  
  # create image set
  observeEvent(input$createSet, {
    req(input$setName)
    
    # check that the name is not already given
    req(!input$setName %in% globalManagers$dataManager()$cciaImageCollection()$cciaObjectNames())
    
    # create new object
    curParams <- list(
      Name = input$setName,
      Type = "Image",
      Class = "CciaImageSet"
    )
    
    # init
    newUID <- globalManagers$projectManager()$genUID()
    newImageSet <- CciaImageSet$new(
      globalManagers$projectManager()$persistentObjectDirectory(newUID, ccidFile = TRUE),
      newUID, initParams = curParams, retrieveState = FALSE)$reactive()
    
    # save
    newImageSet()$saveState()
    
    # add a new set
    globalManagers$dataManager()$cciaImageCollection()$addCciaObjectSet(newImageSet)
  })
  
  ## Generic
  
  ### UI Outputs
  ## Tables
  
  ## Plots
  
  ## Buttons
  
  ## Other
  # attribute filter
  output$attrFilter <- renderUI({
    req(imageSetFilterIDs())
    req(length(imageSetFilterIDs()) > 0)
    
    mapply(
      function(x, i) {
        list(column(
          12,
          createSelectInput(
            session$ns(x), i,
            choices = imageSetFilterValues()[[i]],
            selected = shinyInputValue(x, input),
            multiple = TRUE
          )
        ))
      },
      imageSetFilterIDs(),
      names(imageSetFilterIDs()),
      SIMPLIFY = FALSE)
  })
  
  # control reloading of images
  output$selectSet <- renderUI({
    selectInput(session$ns("selectSet"), NULL, imageSets(),
                selected = input$selectSet)
  })
  
  output$reloadSet <- renderUI({
    actionButton(session$ns("reloadSet"), NULL, icon = icon(btnICON_RELOAD))
  })
  
  ## public functions
  list(
    selectedSet = selectedSet,
    filteredUIDs = filteredUIDs
  )
}
