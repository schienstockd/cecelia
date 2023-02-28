#' @description Server to create canvas plots
#' Plotting adapted from https://github.com/JoachimGoedhart/PlotsOfData
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.plotFlowGatingServer <- function(id, parent, globalManagers) {
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
      # populations to get
      resultParamsPops <- reactive({
        input$resultParamsPops
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # selected ccia object
      cciaObj <- reactive({
        moduleManagers()$imageViewerManager$shownImage()
      })
      
      # selected ccia set
      cciaSet <- reactive({
        moduleManagers()$imageSetManager$selectedSet()
      })
      
      selectedUIDs <- reactive({
        moduleManagers()$selectionManager$selectedUIDs()
      })
      
      # pop type
      popType <- reactive({
        input$resultParamsPopType
      })
      
      # experimental info
      expInfo <- reactive({
        req(cciaSet())
        
        as.data.table(cciaSet()$summary(withSelf = FALSE, fields = c("Attr")))
      })
      
      # generate dataframe from selected image list
      imageData <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        moduleManagers()$imageSetManager$selectedSet()$summary(
          c("Attr"), withSelf = FALSE,
          uIDs = moduleManagers()$imageSetManager$filteredUIDs())
      })
      
      # plot data that is shown
      plotData <- reactive({
        req(summaryPlotData())
        
        summaryPlotData()
      })
      
      # summary data for plot
      summaryPlotData <- reactive({
        req(cciaObj())
        req(resultParamsPops())
        req(all(
          !is.null(input$labelSize),
          !is.null(input$asContours),
          !is.null(input$showPopColours),
          !is.null(input$directLeaves),
          !is.null(input$nRow),
          !is.null(input$nCol)
        ))
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        p1s <- .flowPlotGatedRaster(
          cciaObj(),
          popPath = resultParamsPops(),
          labelSize = input$labelSize,
          asContours = input$asContours,
          showPopColours = input$showPopColours,
          directLeaves = input$directLeaves
        )
        
        progress$close()
        
        req(length(p1s) > 0)
        
        ggpubr::ggarrange(plotlist = p1s, nrow = input$nRow, ncol = input$nCol)
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      ### Observers - RxAction
      ## Event specific
      
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
      
      # plot data
      output$plotData <- renderUI({
        # req(popType())
        req(cciaObj())
        
        # get pop type columns
        popTypePops <- list()
        
        if (!is.null(popType())) {
          popTypePops <- unname(cciaSet()$popPaths(
            uIDs = selectedUIDs(), popType = popType(), includeFiltered = TRUE))
        }
        
        # get pop type columns
        popTypeChoices <- cciaConf()$parameters$popTypes
        
        # create ui elements
        tagList(fluidRow(
          column(
            3,
            tags$label("Parameter plots"),
            selectInput(
              session$ns("resultParamsPopType"), "Population Type",
              choices = .reverseNamedList(popTypeChoices),
              # selected = isolate(popType())
              selected = "flow"
            ),
            createSelectInput(
              session$ns("resultParamsPops"),
              label = "Populations to get",
              choices = popTypePops,
              multiple = FALSE,
              selected = isolate(resultParamsPops())
            )
            # createSelectInput(
            #   session$ns("resultParamsCols"),
            #   label = "Properties",
            #   choices = unname(cciaObj()$labelPropsCols()),
            #   multiple = TRUE,
            #   selected = isolate(resultParamsCols())
            # )
          ),
          column(
            3,
            tags$label("Summary plots")
          )
        ))
      })
      
      # plot params
      output$plotParams <- renderUI({
        tagList(
          h4("Gating"),      
          
          sliderInput(
            session$ns("labelSize"), label = "Label size",
            value = 2, min = 0.2, max = 10, step = 0.2
          ),
          sliderInput(
            session$ns("nRow"), label = "Rows",
            value = 2, min = 1, max = 10, step = 1
          ),
          sliderInput(
            session$ns("nCol"), label = "Columns",
            value = 2, min = 1, max = 10, step = 1
          ),
          checkboxInput(
            session$ns("showPopColours"), label = "Show pop colours", value = FALSE),
          checkboxInput(
            session$ns("asContours"), label = "Show contours", value = TRUE),
          checkboxInput(
            session$ns("directLeaves"), label = "Direct leaves", value = FALSE),
          
          h4("Plot Layout"),
          numericInput(session$ns("plotHeight"), "Height (# pixels): ", value = 480),
          numericInput(session$ns("plotWidth"), "Width (# pixels):", value = 480),
        )
      })
      
      ## Buttons
      
      ## Other
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet", "imageViewer", "plotCharts")
      managerConf = list(
        moduleName = id,
        imageData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        task = list(
          funLabel = "Chart method"
        ),
        plotCharts = list(
          plotData = plotData,
          summaryPlotData = summaryPlotData,
          numUIDs = reactive({1})
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
