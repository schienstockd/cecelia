#' @description Server for cleaning up images
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.cleanupImagesServer <- function(id, parent, globalManagers) {
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
        list()
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
          c("Attr", "ChannelNames"), withSelf = FALSE,
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
        "ui", "input", "selection", "task", "imageSet", "imageViewer")
      managerConf = list(
        moduleName = id,
        imageData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "cleanup_images"
        ),
        task = list(
          funLabel = "Cleanup method",
          taskVarsToAdd = taskVarsToAdd,
          runTaskCombinedSelect = TRUE
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
