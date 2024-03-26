# Server to plot images
createImageViewerManager <- function(
  input, output, session, globalManagers, moduleManagers, managerConf,
  forceReloadDataOnSelect = FALSE) {
  
  # get image from set
  getImageFromSet <- function(uID) {
    curSelected <- NULL
    if (length(moduleManagers()$imageSetManager$selectedSet()$cciaObjects()) > 0) {
      curSelected <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectByUID(uID)
      if (length(curSelected) > 0) {
        curSelected <- curSelected[[1]]()
      }
    }
    
    curSelected
  }
  
  # trigger the image to show
  showImage <- function(uID) {
    imageToShow(uID)
  }
  
  # update image
  updateImage <- function() {
    imageUpdated(runif(1))
  }
  
  ### Reactive values
  imageToShow <- reactiveVal()
  imageUpdated <- reactiveVal()
  imageShown <- reactiveVal()
  
  ### Reactive-like values
  
  ### Reactives - RxCalc
  ## Event specific
  
  showLabelsAsNpArray <- reactive({
    retVal <- FALSE
    
    if ("showLabelsAsNpArray" %in% names(managerConf$imageViewer)) {
      retVal <- managerConf$imageViewer$showLabelsAsNpArray
    }
    
    retVal
  })
  
  # wrap shown image as reactive
  shownImage <- reactive({
    im <- NULL
    
    if (!is.null(imageToShow())) {
      im <- getImageFromSet(imageToShow())
      
      # catch an empty list
      if (length(im) == 0) {
        im <- NULL
      }
    }
    
    im
  })
  
  # reactive that image selection changed
  imageSelected <- eventReactive(c(
    imageToShow(),
    input$showImage
    ), {
    req(imageToShow())
    req(shownImage())
    
    # reset image shown to signify that it is
    # in the process of being loaded
    imageShown(NULL)
    
    # shownImage()
    runif(1)
  })
  
  ## Generic
  
  ### Observers - RxAction
  ## Event specific
  
  # listen to image selection
  observeEvent(input$showImage, {
    req(input$showImage)
    
    imageToShow(input$showImage)
  })
  
  # image selection
  observeEvent(c(
    imageSelected(),
    imageUpdated()
    ), {
    req(imageSelected())
    # req(globalManagers$viewerManager()$getShowViewer())
    req(globalManagers$viewerManager()$viewer())
    
    # remove highlights
    for (x in managerConf$imageData()$uID) {
      removeClass(
        sprintf("showImage_%s", x),
        btnCLASS_WARNING)
    }
    
    # highlight image in table
    addClass(
      sprintf("showImage_%s", shownImage()$getUID()),
      btnCLASS_WARNING)
    
    # load data from current image
    # shownImage()$retrieveState()
    shownImage()$loadData(forceReloadDataOnSelect)
    
    progress <- Progress$new()
    progress$set(message = "Open image ... ", value = 20)
    
    # set task directory
    globalManagers$viewerManager()$viewer()$setTaskDir(
      shownImage()$persistentObjectDirectory()
    )
    
    # open image
    if (DEBUG_SHOW_VIEWER == TRUE && globalManagers$projectManager()$getProjectType() != "flow") {
      globalManagers$viewerManager()$openImage(
        shownImage, managerConf$imageViewer$napariModule,
        showLabelsAsNpArray()
        )
      
      # reset layer properties
      globalManagers$viewerManager()$loadLayerProps()
    }
    
    # set image shown
    imageShown(runif(1))
    
    progress$set(message = "Done ... ", value = 100)
    progress$close()
  })
  
  ## Generic
  
  ### UI Outputs
  ## Tables
  
  ## Plots
  
  ## Buttons
  
  ## Other
  createShowImageColumn <- function(valueNameSelection = FALSE) {
    columnList <- list()
    
    # highlight shown image
    # do not update if the object is changed
    isolate({
      # prepare image classes
      imageClasses <- rep("", nrow(managerConf$imageData()))
      
      if (!is.null(shownImage())) {
        imageClasses[
          shownImage()$getUID() == managerConf$imageData()$uID
        ] <- btnCLASS_WARNING
      }
      
      # get value names for images to show
      if (valueNameSelection == TRUE) {
        # go through images in set
        cciaObjs <- moduleManagers()$imageSetManager$selectedSet()$cciaObjects()
        
        # create column
        columnList <- append(
          columnList,
          list(
            " " = shinyInput(
              "selectInput", session$ns("valueName_"), unname(sapply(cciaObjs, function(x) x()$getUID())),
              initChoices = unname(sapply(cciaObjs, function(x) x()$valueNames("imFilepath"))),
              initSelected = unname(sapply(cciaObjs, function(x) {
                shinyInputValue(
                  paste0("valueName_", x()$getUID()),
                  input,
                  x()$valueDefault("imFilepath")
                )
              }))
            )
          )
        )
      }
    })
    
    columnList <- append(
      columnList,
      list(" " = shinyInput(
        "actionButton", session$ns("showImage_"), managerConf$imageData()$uID,
        initIcons = rep(btnICON_SHOW_IMAGE, nrow(managerConf$imageData())),
        initClasses = imageClasses,
        initOnclick = paste(
          sprintf(
            'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
            session$ns("showImage"),
            managerConf$imageData()$uID
          )
        )
      )),
    )
    
    columnList
  }
  
  ## public functions
  list(
    createShowImageColumn = createShowImageColumn,
    shownImage = shownImage,
    showImage = showImage,
    updateImage = updateImage,
    imageSelected = imageSelected,
    imageUpdated = imageUpdated,
    imageShown = imageShown
  )
}
