#' @description Server to create canvas plots
#' Plotting adapted from https://github.com/JoachimGoedhart/PlotsOfData
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.plotInteractionHeatmapsServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      spatialDT <- reactiveVal()
      popDT <- reactiveVal()
      summaryDT <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      # populations to get
      resultParamsPops <- reactive({
        input$resultParamsPops
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # properties to show
      resultParamsCols <- reactive({
        input$resultParamsCols
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # summary properties to show
      resultSummaryAxisX <- reactive({
        input$resultSummaryAxisX
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      resultSummaryAxisY <- reactive({
        input$resultSummaryAxisY
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
      
      # value name
      valueName <- reactive({
        # DEBUG - need a selection box for this
        "default"
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
      
      # confidence level
      confidencePercentage <- reactive({95})
      confidenceLevel <- reactive({
        req(confidencePercentage())
        confidencePercentage()/100
      })
      
      # categories from summary
      resultParamsCats <- reactive({
        req(popDT())
        
        paramCats <- c()
        
        if (!is.null(resultSummaryAxisY())) {
          if (.cciaStatsTypeIsCategorical(resultSummaryAxisY())) {
            paramCats <- popDT()[, unique(get(resultSummaryAxisY()))]
          }
        }
        
        paramCats
      })
      
      # populations to show
      resultParamsPopsShow <- reactive({
        input$resultParamsPopsShow
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # categories to show
      resultParamsCatsShow <- reactive({
        input$resultParamsCatsShow
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      resultSummaryAxisXCompiled <- reactive({
        req(resultSummaryAxisX())
        req(resultSummaryInteraction())
        
        if (resultSummaryInteraction() != "NONE")
          paste0(resultSummaryAxisX(), ".", resultSummaryInteraction())
        else
          resultSummaryAxisX()
      })
      
      resultSummaryAxisXList <- reactive({
        req(resultSummaryAxisXCompiled())
        
        strsplit(resultSummaryAxisXCompiled(), "\\.")[[1]]
      })
      
      resultSummaryAxisYCat <- reactive({
        req(resultSummaryAxisY())
        
        if (.cciaStatsTypeIsCategorical(resultSummaryAxisY())) {
          paste0(resultSummaryAxisY(), ".cat")
        } else {
          NULL
        }
      })
      
      resultSummaryAxisYCompiled <- reactive({
        req(resultSummaryAxisY())
        
        list(
          mean = paste0(resultSummaryAxisY(), ".mean"),
          median = paste0(resultSummaryAxisY(), ".median"),
          min = paste0(resultSummaryAxisY(), ".min"),
          max = paste0(resultSummaryAxisY(), ".max"),
          meanCiLo = paste0(resultSummaryAxisY(), ".meanCiLo"),
          meanCiHi = paste0(resultSummaryAxisY(), ".meanCiHi"),
          medianCiLo = paste0(resultSummaryAxisY(), ".medianCiLo"),
          medianCiHi = paste0(resultSummaryAxisY(), ".medianCiHi")
        )
      })
      
      resultSummaryInteraction <- reactive({
        input$resultSummaryInteraction
      })
      
      # points data that is shown
      pointsData <- eventReactive(c(
        input$plotOutputTabs,
        popDT(),
        pointsDT(),
        expInfoUpdated(),
        resultParamsPopsShow(),
        resultParamsCatsShow()
      ), {
        req(input$plotOutputTabs)
        req(popDT())
        req(pointsDT())
        req(expInfo())
        
        # TODO version without copy
        DT <- NULL
        if (input$plotOutputTabs == "combined")
          DT <- pointsDT()
        else
          DT <- popDT()[expInfo(), on = .(uID)]
        
        if (!is.null(resultParamsPopsShow()) && length(resultParamsPopsShow()) > 0)
          DT <- DT %>% dplyr::filter(pop %in% resultParamsPopsShow())
        if (!is.null(resultParamsCatsShow()) && length(resultParamsCatsShow()) > 0)
          DT <- DT %>% dplyr::filter(get(resultSummaryAxisYCat()) %in% resultParamsCatsShow())
        
        DT
      })
      
      # points data that is shown
      summaryData <- reactive({
        req(summaryDT())
        
        summaryDT()
      })
      
      # plot data that is shown
      plotData <- reactive({
        req(summaryPlotData())
        
        summaryPlotData()
      })
      
      # summary data for plot
      summaryPlotData <- reactive({
        req(summaryData())
        
        groupA <- paste0(resultSummaryAxisX(), ".from")
        groupB <- paste0(resultSummaryAxisX(), ".to")
        
        # generate plot layers
        p1 <- ggplot(data = summaryData(),
                     aes(x = as.factor(get(groupA)),
                         y = as.factor(get(groupB))))
        
        xlabTitle <- ""
        ylabTitle <- ""
        
        # main tiles
        p1 <- p1 + geom_tile(aes(fill = freq), colour = "white", size = 0.5) +
          viridis::scale_fill_viridis(
            limits = c(0, 100),
            breaks = c(0, 50, 100)
            # labels = c(0, 50, 100)
          )
        
        # format plot
        .formatSummaryPlotData(
          p1, input, xlabTitle = xlabTitle, ylabTitle = ylabTitle)
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      ### Observers - RxAction
      ## Event specific
      
      # listen to image selection
      observeEvent(c(
        selectedUIDs(),
        popType(),
        resultParamsPops()
      ), {
        req(selectedUIDs())
        req(popType())
        req(resultParamsPops())
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        # get population data
        DT <- cciaSet()$popDT(
          popType = popType(),
          uIDs = selectedUIDs(),
          includeFiltered = TRUE,
          completeDT = FALSE,
          replaceNA = TRUE,
          pops = resultParamsPops(),
          popCols = c("label", "pop")
        )
        
        # get interaction data
        sDT <- cciaSet()$spatialDT(
          valueName = valueName(), uIDs = selectedUIDs())
        
        # join pops
        sDT[DT[, c("uID", "label", "pop")],
            on = c("uID", "to" = "label"),
            pop.to := pop]
        sDT[DT[, c("uID", "label", "pop")],
            on = c("uID", "from" = "label"),
            pop.from := pop]
        sDT[DT[, c("uID", "label", "clusters")],
            on = c("uID", "to" = "label"),
            clusters.to := clusters]
        sDT[DT[, c("uID", "label", "clusters")],
            on = c("uID", "from" = "label"),
            clusters.from := clusters]
        # exclude self interactions
        # sDT <- sDT[clusters.to != clusters.from]
        
        progress$close()
        
        popDT(DT)
        spatialDT(sDT)
      })
      
      # create summary DT
      observeEvent(c(popDT(), resultSummaryAxisX(), resultSummaryAxisY()), {
        req(nrow(popDT()) > 0)
        req(resultSummaryAxisX())
        req(resultSummaryAxisY())
        
        groupA <- paste0(resultSummaryAxisX(), ".from")
        groupB <- paste0(resultSummaryAxisX(), ".to")
        
        summaryDT(as.data.table(
          spatialDT() %>%
            # group_by(.dots = c("uID", groupA, groupB)) %>%
            group_by(.dots = c(groupA, groupB)) %>%
            summarise(n = n()) %>%
            mutate(freq = n/sum(n) * 100) %>%
            drop_na() %>%
            ungroup() %>%
            # TODO is there a better way?
            # https://stackoverflow.com/a/67082584
            # complete(uID, !!!syms(groupA), !!!syms(groupB), fill = list(freq = 0))
            complete(!!!syms(groupA), !!!syms(groupB), fill = list(freq = 0))
        ))
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
      
      # plot data
      output$plotData <- renderUI({
        # req(popType())
        req(cciaObj())
        
        # get pop type columns
        popTypePops <- list()
        popTypeCols <- list()
        popCats <- list()
        
        if (!is.null(popType())) {
          popTypePops <- unname(cciaSet()$popPaths(
            uIDs = selectedUIDs(), popType = popType(),
            includeFiltered = TRUE, filterMeasures = c("clusters")))
        }
        
        # get choices for categories
        propCols <- list(
          "Raw interactions" = "raw"
        )
        
        # add pop and clustering to X-axis
        if (length(popDT()) > 0) {
          if ("clusters" %in% colnames(popDT()))
            popTypeCols <- c("clusters", popTypeCols)
          if ("pop" %in% colnames(popDT()))
            popTypeCols <- c("pop", popTypeCols)
        }
        
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
            tags$label("Summary plots"),
            createSelectInput(
              session$ns("resultSummaryAxisX"),
              label = "X Axis",
              choices = popTypeCols,
              multiple = FALSE,
              selected = isolate(resultSummaryAxisX())
            ),
            createSelectInput(
              session$ns("resultSummaryAxisY"),
              label = "Y Axis",
              choices = propCols,
              multiple = FALSE,
              selected = isolate(resultSummaryAxisY())
            )
          )
        ))
      })
      
      # plot params
      output$plotParams <- renderUI({
        # get default inputs
        .formatSummaryPlotDataInputs(session)
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
          # indvPlotData = indvPlotData,
          summaryData = summaryData,
          summaryPlotData = summaryPlotData,
          plotData = plotData,
          numUIDs = reactive({1})
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
