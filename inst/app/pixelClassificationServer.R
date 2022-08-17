#' @description Server for pixel classification
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.pixelClassificationServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      
      # save populations
      clsfSavePops <- function(pops = NULL, purge = FALSE) {
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
      
      ### Reactive values
      viewerUpdatePops <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      # update image automatically when populations are gated
      updateImage <- eventReactive(c(
        cciaObj(), viewerUpdatePops()
      ), {
        req(cciaObj())
        
        # update image
        runif(1)
      })
      
      ## Generic
      
      # population management
      popType <- reactive({
        input$popType
      })
      
      # add pop type to task variables
      taskVarsToAdd <- reactive({
        req(popType())
        
        list(
          popType = popType()
        )
      })
      
      # get boolean classification columns
      booleanClsfCols <- reactive({
        req(moduleManagers()$populationManager$flowPopStats())
        
        statsCols <- colnames(moduleManagers()$populationManager$flowPopStats())
        
        clsfNames <- str_extract(
          statsCols,
          "(?<=cell.cl.bool#clsf.).*"
        )
        
        clsfCols <- statsCols[!is.na(clsfNames)]
        names(clsfCols) <- clsfNames[!is.na(clsfNames)]
        
        clsfCols
      })
      
      # population map
      imPopMap <- eventReactive(c(
        moduleManagers()$imageViewerManager$imageSelected(),
        cciaObj(),
        booleanClsfCols()
      ), {
        req(cciaObj())
        req(booleanClsfCols())
        
        cciaObj()$imPopMap(popType(), includeFiltered = TRUE,
                           filterMeasures = booleanClsfCols())
      })
      
      # population data
      popData <- eventReactive(c(
        imPopMap(),
        booleanClsfCols()
      ), {
        # req(cciaObj())
        # moduleManagers()$populationManager$createPopData()
        
        # create radio buttons for each group of classification
        # ie/ that you can assign classifications to create
        # a gating hierarchy
        req(imPopMap())
        
        # prepare population dataframe
        zeroPops <- matrix(
          0,
          ncol = length(booleanClsfCols()),
          nrow = length(imPopMap()))
        
        # set names
        colnames(zeroPops) <- names(booleanClsfCols())
        rownames(zeroPops) <- names(imPopMap())
        
        # create DF
        popsDF <- as.data.frame(zeroPops)
        popIDs <- rownames(popsDF)
        
        # for every column init a link to change classification association
        clsfCols <- list()
        inputID <- "setClsfForPop"
        
        if (length(imPopMap()) > 0) {
          for (x in booleanClsfCols()) {
            curID <- sprintf("%s_%s_", inputID, x)
            curValues <- unlist(
              lapply(popIDs, function(i) {
                # get Idx of measure
                measureIdx <- which(imPopMap()[[i]]$filterMeasures == x)
                
                if (length(measureIdx) > 0) {
                  if (all(c(TRUE, FALSE) %in% imPopMap()[[i]]$filterValues[[measureIdx]]))
                    2
                  else if (imPopMap()[[i]]$filterValues[[measureIdx]] == TRUE)
                    1
                  else
                    0
                } else 0
              })
            )
            
            # set pop icons
            popIcons <- rep(btnICON_NOTSELECTED, length(popIDs))
            popIcons[curValues == 1] <- btnICON_SELECTED
            popIcons[curValues > 1] <- btnICON_HALFSELECTED
            
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
            clsfCols <- append(
              clsfCols, curCol)
          }
        }
        
        # create dataframe
        popsDFwithClsf <- do.call(cbind, clsfCols)
        
        if (!is.null(popsDFwithClsf)) {
          names(popsDFwithClsf) <- colnames(popsDF)
          
          colnames(popsDFwithClsf) <- sapply(
            str_split(colnames(popsDF), "/"),
            function(x) {
              x[[1]] <- str_replace(x[[1]], "\\.cl", "")
              x <- c(x[[1]], str_split(x[[2]], "_"))
              
              # "<div class = 'vertical' style = 'height: 150px;'>%s</div>",
              paste(x, collapse = "<br/>")
            })
          rownames(popsDFwithClsf) <- rownames(popsDF)
        }
        
        popsDFwithClsf
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
      
      # listen to viewer output
      observeEvent(globalManagers$viewerManager()$viewerOutput(), {
        req(moduleManagers()$imageViewerManager$shownImage())
        
        viewerOutput <- globalManagers$viewerManager()$viewerOutput()
        outputProcessed <- FALSE
        
        # check whether there is something to do
        if ("pixclSavedLabels" %in% names(viewerOutput)) {
          if (length(viewerOutput$pixclSavedLabels) > 0) {
            progress <- shiny::Progress$new()
            progress$set(message = "Save labels ... ", value = 50)
            
            for (x in viewerOutput$pixclSavedLabels) {
              # set labels path
              cciaObj()$setImLabelsFilepath(
                x, valueName = tools::file_path_sans_ext(x),
                invalidate = FALSE)
            }
            
            # save labels to image
            cciaObj()$saveState()
            
            progress$close()
            
            outputProcessed <- TRUE
          }
        }
        
        # tell the viewer that the command was processed
        if (outputProcessed == TRUE) {
          globalManagers$viewerManager()$clearViewerOutput()
        }
      })
      
      # provide population type to input manager
      observeEvent(popType(), {
        req(popType())
        
        moduleManagers()$inputManager$setPopType(popType())
        
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
              choices = if (!is.null(cciaObj$popUtils(popType())))
                unname(cciaObj$popPaths(popType(), includeFiltered = TRUE))
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
        
        # collapse selection box
        js$collapseBox(session$ns("imageTableBox"))
        
        # save pop map
        cciaObj()$savePopMap(popType(), includeFiltered = TRUE)
      })
      
      # reset labels scale
      observeEvent(input$resetLabelsScale, {
        req(cciaObj())
        req(globalManagers$viewerManager()$viewer())
        
        globalManagers$viewerManager()$viewer()$resetLabelsScale("Result of")
      })
      
      # open pixel classifier
      observeEvent(input$openPixcl, {
        req(cciaObj())
        req(globalManagers$viewerManager()$viewer())
        
        globalManagers$viewerManager()$addPixclPane()
      })
      
      # save labels back
      observeEvent(input$saveLabels, {
        req(cciaObj())
        req(globalManagers$viewerManager()$viewer())
        
        # save all labels on the image
        globalManagers$viewerManager()$viewer()$saveLabels(
          file.path(
            cciaObj()$persistentObjectDirectory(),
            cciaConf()$dirs$tasks$labels
          ),
          excludeNames = "^Result of",
          notifyModuleID = "pixclSavedLabels")
      })
      
      # added population
      observeEvent(moduleManagers()$populationManager$addedPops(), {
        req(moduleManagers()$populationManager$addedPops())
        
        pops <- moduleManagers()$populationManager$addedPops()
        
        # add classifiers to populations
        for (i in names(pops)) {
          x <- pops[[i]]
          
          cciaObj()$setPopAttr(
            popType(), i, list(
              # TODO this does not work when there are multiple adata files
              valueName = attr(cciaObj()$valueNamesForPopType(popType()), "default"),
              filterMeasures = as.list(unname(booleanClsfCols())),
              filterFuns = as.list(rep("eq", length(booleanClsfCols()))),
              filterValues = as.list(rep(FALSE, length(booleanClsfCols())))
            ), includeFiltered = TRUE, invalidate = TRUE
          )
        }
        
        # save populations
        clsfSavePops(
          moduleManagers()$populationManager$addedPops()
        )
        
        # save pop map
        cciaObj()$savePopMap(popType(), includeFiltered = TRUE, invalidate = FALSE)
        
        # udate viewer
        viewerUpdatePops(runif(1))
      })
      
      # set classification for population
      observeEvent(input$setClsfForPop, {
        req(input$setClsfForPop)
        req(cciaObj())
        
        # set classification for population
        # split into population and classification column
        params <- str_split(
          input$setClsfForPop, pattern = "\\.", n = 2)[[1]]
        popID <- params[[1]]
        clsfCol <- params[[2]]
        
        popMap <- imPopMap()
        
        # get idx of column from measures
        clsfColIdx <- which(popMap[[popID]]$filterMeasures == clsfCol)
        
        # add to population
        if (length(clsfColIdx) > 0) {
          # toggle FALSE to TRUE to TRUE/FALSE to FALSE ...
          if (all(c(TRUE, FALSE) %in% popMap[[popID]]$filterValues[[clsfColIdx]]))
            popMap[[popID]]$filterValues[[clsfColIdx]] <- list(FALSE)
          else if (popMap[[popID]]$filterValues[[clsfColIdx]] == TRUE)
            popMap[[popID]]$filterValues[[clsfColIdx]] <- list(TRUE, FALSE)
          else
            popMap[[popID]]$filterValues[[clsfColIdx]] <- list(TRUE)
        } else {
          # add to measures
          popMap[[popID]]$filterMeasures <- append(
            popMap[[popID]]$filterMeasures,
            list(clsfCol)
          )
          
          # add to values
          popMap[[popID]]$filterValues <- append(
            popMap[[popID]]$filterValues,
            list(TRUE)
          )
          
          # add to functions
          popMap[[popID]]$filterFuns <- append(
            popMap[[popID]]$filterFuns,
            list("eq")
          )
          
          clsfColIdx <- length(popMap[[popID]]$filterMeasures)
        }
        
        # set mapping
        cciaObj()$setImPopMap(popType(), popMap, mergeMap = TRUE)
        
        # save populations
        clsfSavePops(popMap[[popID]]$path)
        
        # save pop map
        cciaObj()$savePopMap(popType(), invalidate = FALSE, includeFiltered = TRUE)
        
        # update manually to prevent redrawing of table
        # shinyjs::html(btnID, btnLabel)
        labelID <- sprintf(
          "setClsfForPop_%s_%s", clsfCol, popID)
        
        # TODO why does the icon assignment not work .. ?
        if (all(c(TRUE, FALSE) %in% popMap[[popID]]$filterValues[[clsfColIdx]]))
          shinyjs::html(labelID, htmlIcon(btnICON_HALFSELECTED))
        else if (popMap[[popID]]$filterValues[[clsfColIdx]] == TRUE)
          shinyjs::html(labelID, htmlIcon(btnICON_SELECTED))
        else
          shinyjs::html(labelID, htmlIcon(btnICON_NOTSELECTED))
        
        # udate viewer
        viewerUpdatePops(runif(1))
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
          choices = reverseNamedList(choices),
          selected = choices[[globalManagers$projectManager()$getProjectType()]]
          )
      })
      
      ## Other
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet",
        "imageViewer", "population")
      managerConf = list(
        moduleName = id,
        imageData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "pixel_classification"
        ),
        task = list(
          funLabel = "Pixel classification method",
          taskVarsToAdd = taskVarsToAdd,
          runTaskCombinedSelect = TRUE
        ),
        population = list(
          imPopMap = imPopMap,
          popData = popData,
          popData = popData,
          popType = popType,
          updateImage = updateImage,
          enableFilterPopulation = TRUE,
          popsFilteredFromValueName = TRUE
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
