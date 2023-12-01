#' @description Server to import images
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.importImagesServer <- function(id, parent, globalManagers) {
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
          disable("imagesToImport")
          disable("foldersToImport")
          disable("setupLabServer")
          disable("deleteSet")
        } else {
          enable("imagesToImport")
          enable("foldersToImport")
          enable("setupLabServer")
          enable("deleteSet")
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
      
      # show modal to select image folders
      showSelectFolders <- function(failed = c()) {
        # get selected folder
        selectedFolder <- joinSelectedPath(input$foldersToImport)
        imFolders <- list.dirs(selectedFolder, recursive = FALSE)
        names(imFolders) <- basename(imFolders)
        
        # get directories and build checkboxes
        modalDialog(
          fluidRow(
            column(
              12,
              if ("selectedFolders" %in% failed) {
                tags$div("Enter correct path",
                         class = errCLASS)
              },
              checkboxGroupInput(
                session$ns("selectedFolders"), "Select images to import", imFolders, width = "100%"),
              actionButton(
                session$ns("selectedFoldersSubmit"), "Import images")
            )
          ),
          easyClose = TRUE, size = "l"
        )
      }
      
      # import images
      observeEvent(input$selectedFoldersSubmit, {
        # create new images
        newImages <- list()
        
        for (x in input$selectedFolders) {
          # TODO this assumes that the first file is the one
          # that is used to create the image
          curParams <- list(
            Name = basename(x),
            Type = "Image",
            Class = "CciaImage",
            Meta = list(
              "oriFilepath" = list.files(x, full.names = TRUE, no.. = TRUE)[[1]]
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
        
        # close modal
        removeModal()
      })
      
      # import folders
      # TODO this is specific to Thorlabs images at the moment
      observeEvent(input$foldersToImport, {
        req(input$foldersToImport)
        req("path" %in% names(input$foldersToImport))
        
        # show modal for user to select images
        showModal(showSelectFolders())
      })
      
      # delete uID
      observeEvent(input$deleteUID, {
        req(input$deleteUID)
        
        showModal(modalDialog(
          tagList(h3("Are you sure?")), 
          title = "Delete Image",
          footer = tagList(actionButton(session$ns("deleteUIDConfirmed"), "Delete image"),
                           modalButton("Cancel")
          )
        ))
      })
      
      # observeEvent(input$deleteUID, {
      observeEvent(input$deleteUIDConfirmed, {
        req(input$deleteUID)
        
        progress <- Progress$new()
        
        # remove content from HPC
        if (globalManagers$projectManager()$useHPC() == TRUE) {
          progress$set(message = "Delete HPC files ... ", value = 50)
          
          globalManagers$projectManager()$deleteHPCpersistentObjectDirectory(
            input$deleteUID, removeZero = TRUE)
        }
        
        progress$set(message = "Delete local files ... ", value = 80)
        
        # remove from set
        moduleManagers()$imageSetManager$selectedSet()$removeCciaObjectByUID(
          input$deleteUID, removeZero = TRUE)
        
        progress$close()
        
        # autosave project
        globalManagers$projectManager()$doProjectBookmark()
        
        removeModal()
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
      
      shinyDirChoose(
        input, "foldersToImport",
        # roots = cciaConf()$volumes,
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
