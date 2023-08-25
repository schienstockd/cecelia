#' @description Server to create canvas plots
#' Plotting adapted from https://github.com/JoachimGoedhart/PlotsOfData
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.plotPopDensitiesServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      popDT <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      # listen to image selection
      # observeEvent(moduleManagers()$selectionManager$selectedUIDs(), {
      observeEvent(c(
        selectedUIDs(),
        popType(),
        resultParamsPops()
      ), {
        # req(moduleManagers()$selectionManager$selectedUIDs())
        req(selectedUIDs())
        req(popType())
        req(resultParamsPops())
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        # popDT(moduleManagers()$imageSetManager$selectedSet()$popDT(
        DT <- cciaSet()$popDT(
          popType = popType(),
          uIDs = selectedUIDs(),
          includeFiltered = TRUE,
          # completeDT = TRUE,
          # replaceNA = TRUE,
          pops = resultParamsPops()
        )
        
        # add density colour per pop
        # TODO is this the right spot for this?
        DT[, density := .flowColours(.SD$centroid_x, .SD$centroid_y),
           by = c("uID", "pop"), .SDcols = c("centroid_x", "centroid_y")]
        
        progress$close()
        
        popDT(DT)
      })
      
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
        req(popDT())
        req(resultParamsPops())
        req(all(
          !is.null(input$portraitMode),
          !is.null(input$nRow),
          !is.null(input$nCol)
        ))
        
        # make sure that it only requests data when selected
        req(globalManagers$input$sidebar() == session$ns(c()))
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        # plot density maps for individual populations
        p1s <- list()
        
        for (i in unique(popDT()$uID)) {
          local({
            x <- popDT()[uID == i]
            
            # trim population paths
            if (input$trimPopPaths >= 0) {
              x[, pop := .flowTrimPath(.SD$pop, input$trimPopPaths),
                by = c("pop"), .SDcols = c("pop")]
            }
            
            axisX <- "centroid_x"
            axisY <- "centroid_y"
            
            # can you orientate them in the same direction?
            if (input$portraitMode == TRUE) {
              if (max(x$centroid_x) > max(x$centroid_y)) {
                axisX <- "centroid_y"
                axisY <- "centroid_x"
              }
            } else {
              if (max(x$centroid_x) < max(x$centroid_y)) {
                axisX <- "centroid_y"
                axisY <- "centroid_x"
              }
            }
            
            # p1s[[i]] <<- ggplot(x, aes(get(axisX), -get(axisY))) +
            p1 <- ggplot(x, aes(get(axisX), get(axisY))) +
              theme_classic() +
              geom_point(colour = x$density) +
              facet_grid(uID~pop) +
              theme(
                strip.text = element_text(size = input$labelSize),
              )
              # plotThemeDark(angle = 0) +
            
            if (input$darkThemeMode == TRUE) {
              p1 <- p1 + plotThemeDark(angle = 0, fontSize = input$labelSize)
            }
            
            # further theme
            p1 <- p1 + 
              coord_fixed() +
              theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "none",
                strip.background = element_blank()
              ) +
              scale_x_continuous(expand = c(0, 0)) +
              scale_y_continuous(expand = c(0, 0))
            
            p1s[[i]] <<- p1
            
            # p1s[[i]] <<- .formatSummaryPlotData(
            #   p1, input, xlabTitle = "", ylabTitle = "")
          })
        }
        
        progress$close()
        
        req(length(p1s) > 0)
        req(input$nRow * input$nCol >= length(p1s))
        
        p2 <- ggpubr::ggarrange(plotlist = p1s, nrow = input$nRow, ncol = input$nCol)
        
        # add background
        if (input$darkThemeMode == TRUE)
          p2 <- p2 + theme(plot.background = element_rect(fill = "#222222", color = NA))
        
        p2
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
        
        # add root
        popTypePops <- c("root", popTypePops)
        
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
              selected = isolate(popType())
            ),
            createSelectInput(
              session$ns("resultParamsPops"),
              label = "Populations to get",
              choices = popTypePops,
              multiple = TRUE,
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
          h4("Population densities"),      
          
          sliderInput(
            session$ns("labelSize"), label = "Label size",
            value = 12, min = 1, max = 20, step = 1
          ),
          sliderInput(
            session$ns("trimPopPaths"), label = "Trim Population names",
            value = 0, min = -1, max = 6, step = 1
          ),
          sliderInput(
            session$ns("nRow"), label = "Rows",
            value = 2, min = 1, max = 20, step = 1
          ),
          sliderInput(
            session$ns("nCol"), label = "Columns",
            value = 2, min = 1, max = 20, step = 1
          ),
          checkboxInput(
            session$ns("portraitMode"), label = "Portrait Mode", value = FALSE),
          checkboxInput(
            session$ns("darkThemeMode"), label = "Dark Theme", value = TRUE),
          
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
