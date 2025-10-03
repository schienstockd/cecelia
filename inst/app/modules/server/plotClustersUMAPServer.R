#' @description Server to create canvas plots
#' Plotting adapted from https://github.com/JoachimGoedhart/PlotsOfData
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.plotClustersUMAPServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      popDT <- reactiveVal()
      summaryDT <- reactiveVal()
      clusterColPal <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      # populations to get
      resultParamsPops <- reactive({
        input$resultParamsPops
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # clustering to get
      resultParamsClustering <- reactive({
        input$resultParamsClustering
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
      pointsData <- reactive({
        req(popDT())
        
        popDT()
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
        req(pointsData())
        req(resultSummaryAxisY())
        
        # generate plot layers
        # get mean positions of clusters
        meanClusterPos <- pointsData() %>%
          group_by(get(resultSummaryAxisY())) %>%
          summarise(
            UMAP_1 = mean(UMAP_1),
            UMAP_2 = mean(UMAP_2)
          ) %>% dplyr::rename(clust = "get(resultSummaryAxisY())")
        
        if (resultSummaryAxisY() == "pop") {
          # get population colors
          colPal <- sapply(
            unique(pointsData()[[resultSummaryAxisY()]]),
            function(x) {
              y <- cciaObj()$popAttr(
                popType(), "colour", popPath = x, includeFiltered = TRUE, selectedOnly = TRUE)
              
              if (length(y) > 0)
                y[[1]]
              else
                "grey"
            })
        } else {
          colPal <- clusterColPal()
        }
        
        # plot UMAP
        p1 <- ggplot(pointsData(), aes(UMAP_1, UMAP_2)) +
          geom_point(aes(color = get(resultSummaryAxisY())), size = input$pointSize) +
          theme_classic() +
          scale_color_manual(values = colPal) 
        
        xlabTitle <- ""
        ylabTitle <- ""
        
        # format plot
        .formatSummaryPlotData(
          p1, input, xlabTitle = xlabTitle, ylabTitle = ylabTitle) +
          ggrepel::geom_label_repel(
            data = meanClusterPos,
            aes(label = clust),
            size = input$labelSize,
            label.size = input$labelBorder,
            alpha = input$labelAlpha,
            color = "black"
          ) +
          theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.justification = "right",
            # legend.position = "bottom"
            legend.position = "none",
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
          )
        
        # add facet wrap for category?
        # p1 <- p1 + facet_grid(.~cat) +
        # p1 <- p1 + facet_wrap(.~cat, nrow = 1, scales = "free_x") +
        #   theme(
        #     strip.background = element_rect(fill = NA, color = "black", size = 2),
        #     strip.text.x = element_text(color = "black")
        #   )
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      ### Observers - RxAction
      ## Event specific
      
      # listen to image selection
      observeEvent(c(
        selectedUIDs(),
        resultParamsPops(),
        resultParamsClustering()
      ), {
        req(cciaSet())
        req(selectedUIDs())
        # req(resultParamsPops())
        
        # make sure that it only requests data when selected
        req(globalManagers$input$sidebar() == session$ns(c()))
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        if (length(popType()) > 0 && popType() == "live") {
          if (length(resultParamsClustering()) > 0 && resultParamsClustering() != "")
            popDT(as.data.table(cciaEnv()$LabelPropsUtils(
              cciaSet()$persistentObjectDirectory(),
              value_name = resultParamsClustering())$label_props_view()$as_df()))
        } else if (length(resultParamsPops()) > 0) {
          popDT(cciaSet()$popDT(
            popType = popType(),
            uIDs = selectedUIDs(),
            includeFiltered = TRUE,
            completeDT = TRUE,
            replaceNA = TRUE,
            pops = resultParamsPops()
          ))
        }
        
        # get colours for clusters
        if (length(popDT()) > 0 && nrow(popDT()) > 0) {
          clusterColPal(
            randomcoloR::distinctColorPalette(length(unique(
              popDT()$clusters))))
        }
        
        progress$close()
      })
      
      # create summary DT
      observeEvent(c(popDT(), resultSummaryAxisY()), {
        req(popDT())
        req(resultSummaryAxisY())
        
        # make summary
        # TODO data.table only
        summaryDT(as.data.table(
          popDT() %>%
            # left_join(expInfo()) %>%
            group_by(pop, get(resultSummaryAxisY())) %>%
            summarise(n = n()) %>%
            dplyr::rename_with(
              ~ c(paste0(resultSummaryAxisY(), ".cat")),
              all_of(c("get(resultSummaryAxisY())"))
            ) %>%
            mutate(
              freq = n/sum(n),
            )
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
        clusteringFiles <- c()
        
        if (!is.null(popType())) {
          popTypePops <- unname(cciaSet()$popPaths(
            uIDs = selectedUIDs(), popType = popType(),
            includeFiltered = TRUE, includeRoot = TRUE))
          popTypeCols <- cciaObj()$labelPropsCols(popType = popType())
          
          # focus only on categorical
          if (length(popTypeCols) > 0)
            popTypeCols <- popTypeCols[sapply(popTypeCols, .cciaStatsTypeIsCategorical)]
          
          # get clustering files if needed
          if (popType() == "live") {
            clusteringFiles <- list.files(
              file.path(cciaSet()$persistentObjectDirectory(),
                        cciaConf()$dirs$tasks$labelProps),
              pattern = ".sc.")
            clusteringFiles <- str_extract(clusteringFiles, "^.*[^.h5ad]")
          }
        }
        
        # get choices for categories
        propCols <- c()
        
        # add pop and clustering to X-axis
        if (length(popDT()) > 0) {
          # this really only makes sense for live
          if (popType() == "live")
            propCols <- cciaObj()$labelPropsCols()
          
          if ("clusters" %in% colnames(popDT()))
            propCols <- c("clusters", propCols)
          if ("pop" %in% colnames(popDT()))
            propCols <- c("pop", propCols)
          
          # make sure that columns exist
          propCols <- propCols[propCols %in% colnames(popDT())]
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
            ),
            createSelectInput(
              session$ns("resultParamsClustering"),
              label = "Clustering to get",
              choices = clusteringFiles,
              multiple = FALSE,
              selected = isolate(resultParamsClustering())
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
            # createSelectInput(
            #   session$ns("resultSummaryAxisX"),
            #   label = "X Axis",
            #   choices = popTypeCols,
            #   multiple = TRUE,
            #   selected = isolate(resultSummaryAxisX())
            #   # selected = c(
            #   #   "live.cell.hmm.state.shape", "live.cell.hmm.state.movement")
            # ),
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
        append(
          tagList(
            h4("Clustering"), 
            
            sliderInput(
              session$ns("pointSize"), label = "Point size",
              value = 1, min = 0, max = 10, step = 1
            ),
            sliderInput(
              session$ns("labelSize"), label = "Label size",
              value = 10, min = 0, max = 20, step = 1
            ),
            sliderInput(
              session$ns("labelBorder"), label = "Label border",
              value = 1, min = 0, max = 10, step = 0.5
            ),
            sliderInput(
              session$ns("labelAlpha"), label = "Label alpha",
              value = 1, min = 0, max = 1, step = 0.05
            ),
          ),
          # get default inputs
          .formatSummaryPlotDataInputs(session)
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
        selectionData = imageData,
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
