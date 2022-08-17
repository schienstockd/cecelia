#' @description Server to import flow
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.importFlowServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      
      # generate dataframe from selected image list
      imageData <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        moduleManagers()$imageSetManager$selectedSet()$summary(
          c("Name"), withSelf = FALSE,
          uIDs = moduleManagers()$imageSetManager$filteredUIDs())
      })
      
      ### Observers - RxAction
      ## Event specific
      # selected image set
      observeEvent(moduleManagers()$imageSetManager$selectedSet(), {
        # toggle selection
        if (is.null(moduleManagers()$imageSetManager$selectedSet())){
          shinyjs::disable("imagesToImport")
          shinyjs::disable("setupLabServer")
          shinyjs::disable("deleteSet")
        } else {
          shinyjs::enable("imagesToImport")
          shinyjs::enable("setupLabServer")
          shinyjs::enable("deleteSet")
        }
      }, ignoreNULL = FALSE)
      
      # import images
      observeEvent(input$imagesToImport, {
        req(input$imagesToImport)
        # check that a file was selected
        req("files" %in% names(input$imagesToImport))
        
        newFiles <- joinSelectedFiles(input$imagesToImport)

        # create new images
        newImages <- list()
        for (curFile in newFiles) {
          curParams <- list(
            Name = basename(curFile),
            Type = "Image",
            Class = "CciaImage",
            Meta = list(
              "oriFilepath" = curFile
              )
            )
          
          # init
          newUID <- globalManagers$projectManager()$genUID()
          newImage <- CciaImage$new(
            globalManagers$projectManager()$persistentObjectDirectory(newUID, ccidFile = TRUE),
            newUID, initParams = curParams, retrieveState = FALSE)$reactive()
          
          # add to list
          newImages <- append(newImages, newImage)
        }
        
        # add to set
        moduleManagers()$imageSetManager$selectedSet()$addCciaObjects(newImages)
        
        # autosave project
        globalManagers$projectManager()$doProjectBookmark()
      })
      
      # delete uID
      observeEvent(input$deleteUID, {
        req(input$deleteUID)
        
        # remove content from HPC
        if (globalManagers$projectManager()$useHPC() == TRUE) {
          progress <- shiny::Progress$new()
          progress$set(message = "Delete HPC files ... ", value = 50)
          
          globalManagers$projectManager()$deleteHPCpersistentObjectDirectory(
            input$deleteUID, removeZero = TRUE)
          
          progress$close()
        }
        
        # remove from set
        moduleManagers()$imageSetManager$selectedSet()$removeCciaObjectByUID(
          input$deleteUID, removeZero = TRUE)
        
        # autosave project
        globalManagers$projectManager()$doProjectBookmark()
      })
      
      ## Generic
      
      ### UI Outputs
      ## Tables
      output$imageTable <- DT::renderDataTable({
        req(imageData())
        req(nrow(imageData()) > 0)
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          moduleManagers()$selectionManager$createSelectionColumn(),
          " " = shinyInput(
            "actionButton", session$ns("deleteUID_"), imageData()$uID,
            initLabels = rep(btnLABEL_DELETE, nrow(imageData())),
            class = btnCLASS_IMPORTANT,
            initOnclick = paste(
              sprintf(
                'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
                session$ns("deleteUID"),
                imageData()$uID
              )
            )
          ),
          imageData()
          # moduleManagers()$taskManager$createTaskDataTableColumns()
        ))
      })
      
      ## Plots
      
      ## Buttons
      
      ## Other
      shinyFileChoose(
        input, "imagesToImport",
        roots = shinyFiles::getVolumes(),
        session = session)
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet")
      managerConf = list(
        moduleName = id,
        imageData = imageData,
        imageSet = list(
          enableAddition = TRUE,
          enableDeletion = TRUE
        ),
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        task = list(
          funLabel = "Import method"
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
