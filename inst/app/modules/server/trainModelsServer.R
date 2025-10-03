#' @description Server for training models
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.trainModelsServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      
      ### Reactive-like values
      
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
      
      # listen to data selection
      # this will also trigger when the same image is chosen again
      # selected - user selects
      # shown - the image shown
      observeEvent(moduleManagers()$imageViewerManager$imageSelected(), {
        req(cciaObj())
        
        # collapse selection box
        # js$collapseBox(session$ns("imageTableBox"))
      })
      
      # save labels back
      observeEvent(input$saveLabels, {
        req(cciaObj())
        req(globalManagers$viewerManager()$viewer())
        
        # get all labels
        # was the layer created or loaded from previous labelling?
        labelValueNames <- cciaObj()$valueNames("imLabelsFilepath")
        layerNames <- if (length(labelValueNames) > 0) labelValueNames else c("Labels")
        
        # go through labels and save
        for (x in layerNames) {
          if (x == "Labels") {
            # set labels path
            labelsPath <- file.path(
              cciaObj()$persistentObjectDirectory(),
              cciaConf()$dirs$tasks$labels,
              paste0('manual.zarr')
            )
            
            # save into object
            cciaObj()$setImLabelsFilepath(
              labelsPath, valueName = "manual", invalidate = FALSE)
            cciaObj()$saveState()
            
            layerName <- "Labels"
          } else {
            # set labels path
            labelsPath <- file.path(
              cciaObj()$persistentObjectDirectory(),
              cciaConf()$dirs$tasks$labels,
              paste0(x, '.zarr')
            )
            
            layerName <- sprintf("(%s) base Labels", x)
          }
          
          # save labels to currently selected image as zarr
          globalManagers$viewerManager()$viewer()$saveLabels(
            labelsPath, layerName = layerName)
        }
      })
      
      # observe controls for image selection
      observeEvent(input$prevImage, {
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        # select previous image
        if (is.null(moduleManagers()$imageViewer$shownImage())) {
          uID <- imageData()[nrow(imageData()), "uID"]
        } else {
          curIdx <- as.integer(rownames(imageData()[
            imageData()$uID == moduleManagers()$imageViewer$shownImage()$getUID(),]))
          
          # select previous
          if (curIdx > 1) 
            curIdx <- curIdx - 1
          
          # set uID
          uID <- imageData()[curIdx, "uID"]
        }
        
        moduleManagers()$imageViewer$showImage(uID)
      })
      
      # select next image
      observeEvent(input$nextImage, {
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        # select previous image
        if (is.null(moduleManagers()$imageViewer$shownImage())) {
          uID <- imageData()[1, "uID"]
        } else {
          curIdx <- as.integer(rownames(imageData()[
            imageData()$uID == moduleManagers()$imageViewer$shownImage()$getUID(),]))
          
          # browser()
          
          # select previous
          if (curIdx < nrow(imageData())) 
            curIdx <- curIdx + 1
          
          # set uID
          uID <- imageData()[curIdx, "uID"]
        }
        
        moduleManagers()$imageViewer$showImage(uID)
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
      
      ## Other
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet",
        "imageViewer")
      managerConf = list(
        moduleName = id,
        selectionData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "train_models",
          showLabelsAsNpArray = TRUE
        ),
        task = list(
          funLabel = "Training method",
          taskVarsToAdd = taskVarsToAdd,
          runTaskCombinedSelect = TRUE
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
