#' @description Server for spatial analysis
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.spatialAnalysisServer <- function(id, parent, globalManagers) {
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
      
      # population data
      popData <- reactive({
        req(cciaObj())
        
        moduleManagers()$populationManager$createPopData()
      })
      
      # selected ccia object
      cciaObj <- reactive({
        moduleManagers()$imageViewerManager$shownImage()
      })
      
      # number of plots
      numFlowPlots <- reactive({
        input$numFlowPlots
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
        
        # set init flag
        moduleManagers()$flowPlotManager$initGatingBoxPlots(TRUE)
        
        # init all populations from root
        moduleManagers()$flowPlotManager$flowSavePops("root", purge = TRUE)
        
        # save pop map
        cciaObj()$savePopMap(popType(),
                             includeFiltered = TRUE)
      })
      
      # added population
      observeEvent(moduleManagers()$populationManager$addedPops(), {
        req(moduleManagers()$populationManager$addedPops())
        popInfo <- moduleManagers()$populationManager$addedPops()
        
        # update traces where population is shown
        for (x in moduleManagers()$flowPlotManager$flowGatingPlots()) {
          xIDs <- x()$getBoxIDs()
          popLeaves <- cciaObj()$popLeaves(popType(), x()$getPlotPopPath(),
                                           includeFiltered = TRUE)
          
          # update subpopulations
          if (any(names(popInfo) %in% names(popLeaves))) {
            if (length(popLeaves) > 0) {
              popLeaves <- .reverseNamedList(popLeaves)
            }
            
            updateSelectInput(
              session, xIDs$popLeaves,
              # choices = cciaObj()$flowGatingSet()$popLeaves(
              #   flowPlot()$getPlotPopPath()),
              choices = popLeaves,
              selected = x()$getPlotPopLeaves()
            )
          }
        }
        
        # save all filtered populations
        # for (x in popInfo) ..
        moduleManagers()$flowPlotManager$flowSavePops(filteredOnly = TRUE)
        
        # save population map
        cciaObj()$savePopMap(popType(), includeFiltered = TRUE)
      })
      
      # DEBUG plots are rendered
      observeEvent(input$debugPlotsRendered, {
        moduleManagers()$flowPlotManager$debugPlotsRendered()
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
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet",
        "imageViewer", "population", "shapes", "flowPlot")
      managerConf = list(
        moduleName = id,
        imageData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "spatial_analysis"
        ),
        task = list(
          funLabel = "Spatial analysis method",
          taskVarsToAdd = taskVarsToAdd
        ),
        population = list(
          popData = popData,
          popType = popType,
          updateImage = updateImage,
          enableFilterPopulation = TRUE
        ),
        shapes = list(
        ),
        flowPlot = list(
          popType = popType,
          numFlowPlots = numFlowPlots,
          flowUseFlowColours = reactive(TRUE),
          flowMarkerOpacity = reactive(1.0),
          showFilters = TRUE
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
