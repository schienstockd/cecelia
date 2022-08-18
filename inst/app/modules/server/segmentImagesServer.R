#' @description Server to segment images
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.segmentImagesServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Classic variables
      ### Functions
      
      # # segment viewpoint
      # segmentViewpoint <- function() {
      #   progress <- Progress$new()
      #   progress$set(message = "Save viewpoint ... ", value = 10)
      #   
      #   if (taskFunction() == "donbloSeg") {
      #     # create new viewpoint
      #     globalManagers$viewerManager()$viewer()$saveVpAsTif(
      #       input$vpXYpadding, input$vpZpadding,
      #       input$vpXYsnap, input$vpZsnap
      #     )
      #   } else if (taskFunction() %in% c("stardistSeg")) {
      #     globalManagers$viewerManager()$viewer()$saveVpAsVar(
      #       input$vpXYpadding, input$vpZpadding,
      #       input$vpXYsnap, input$vpZsnap
      #     )
      #   }
      #   
      #   progress$set(message = "Segment viewpoint ... ", value = 50)
      #   
      #   # save parameters
      #   moduleManagers()$taskManager$saveFunParams()
      #   
      #   if (taskFunction() == "donbloSeg") {
      #     # segment viewpoint
      #     ImagejUtils$new()$runDonblo(
      #       vpCubeFilepath(),
      #       blobChannels = funParams()$blobChannels,
      #       donutChannels = funParams()$donutChannels,
      #       cellRadius = funParams()$cellRadius,
      #       gaussianFilter = funParams()$cellGaussianFilter,
      #       medianFilter = funParams()$cellMedianFilter,
      #       minimumFilter = funParams()$cellMinimumFilter,
      #       maximumFilter = funParams()$cellMaximumFilter,
      #       detectionThreshAdj = funParams()$thresholdAdj,
      #       # labelExpansion = funParams()$labelExpansion,
      #       rollingRadius = funParams()$cellRollingBall
      #     )
      #   } else if (taskFunction() == "stardistSeg") {
      #     # .. Check options for StarDist
      #     globalManagers$viewerManager()$viewer()$segVpWith(
      #       "napariUtils.imCubeSeg", "stardist", list(
      #         nucleiChannels = funParams()$nucleiChannel,
      #         labelExpansion = funParams()$labelExpansion
      #       )
      #     )
      #   }
      #   
      #   progress$set(message = "Show segmentation ... ", value = 90)
      #   
      #   # show segmentation layer
      #   globalManagers$viewerManager()$viewer()$removeSegmentationLayer()
      #   
      #   if (taskFunction() == "donbloSeg") {
      #     globalManagers$viewerManager()$viewer()$showSegmentationLayer(
      #       labelsPath = paste0(
      #         tools::file_path_sans_ext(vpCubeFilepath()),
      #         ".seg.tif"
      #       )
      #     )
      #   } else if (taskFunction() %in% c("stardistSeg")) {
      #     globalManagers$viewerManager()$viewer()$showSegmentationLayer(
      #       labelsDat = "napariUtils.imCubeSeg"
      #     )
      #   }
      #   
      #   progress$close()
      # }
      
      ### Reactive values
      # general
      
      # # cube filepath
      # vpCubeFilepath <- reactive({
      #   retVal = NULL
      # 
      #   if (!is.null(moduleManagers()$imageViewerManager$shownImage())) {
      #     retVal <- file.path(
      #       moduleManagers()$imageViewerManager$shownImage()$persistentObjectDirectory(),
      #       "vpCube.tif"
      #     )
      #   }
      # 
      #   retVal
      # })
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      
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
      
      # listen to viewer output
      observeEvent(globalManagers$viewerManager()$viewerOutput(), {
        req(moduleManagers()$imageViewerManager$shownImage())
        
        viewerOutput <- globalManagers$viewerManager()$viewerOutput()
        outputProcessed <- FALSE
        
        # check whether there is something to do
        if ("segment" %in% names(viewerOutput)) {
          if (viewerOutput$segment == "viewpoint") {
            # TODO Think about a better way to do this
            # segmentViewpoint()
            outputProcessed <- TRUE
          }
        }
        
        # tell the viewer that the command was processed
        if (outputProcessed == TRUE){
          globalManagers$viewerManager()$clearViewerOutput()
        }
      })
      
      # # segment viewpoint with selected method
      # observeEvent(input$segmentVp, {
      #   segmentViewpoint()
      # })
      
      ## Generic
      
      ### UI Outputs
      ## Tables
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
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet", "imageViewer")
      managerConf = list(
        moduleName = id,
        imageData = imageData,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "segmentation"
        ),
        task = list(
          funLabel = "Segmentation method",
          runTaskOnSetSelect = TRUE
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
