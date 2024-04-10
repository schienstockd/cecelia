#' @description Server for behaviour analysis
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.behaviourAnalysisServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      # population DT
      popDT <- reactiveVal()
      
      # flush cache?
      resultFlushCache <- reactiveVal()
      
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
      # populations to show
      resultParamsPops <- reactive({
        input$resultParamsPops
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # properties to show
      resultParamsCols <- reactive({
        input$resultParamsCols
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      resultParamsColName <- reactive({
        input$resultParamsColName
      })
      
      # summary properties to show
      resultSummaryAxisX <- reactive({
        input$resultSummaryAxisX
      })
      
      resultSummaryInteraction <- reactive({
        input$resultSummaryInteraction
      })
      
      resultSummaryFill <- reactive({
        input$resultSummaryFill
      })
      
      # experiment info
      expInfo <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        
        # get experimental info for set
        moduleManagers()$imageSetManager$selectedSet()$summary(
          withSelf = FALSE, fields = c("Attr")
        )
      })
      
      # population management
      popType <- reactive({
        "live"
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
      
      # population DT for set
      observeEvent(c(
        moduleManagers()$imageSetManager$selectedSet(),
        moduleManagers()$selectionManager$selectedUIDs(),
        moduleManagers()$imageViewerManager$imageUpdated(),
        resultParamsPops(),
        expInfo()
      ), {
        req(moduleManagers()$imageSetManager$selectedSet())
        req(expInfo())
        req(resultParamsPops())
        req(length(moduleManagers()$selectionManager$selectedUIDs()) > 0)
        
        # get uIDs
        uIDs <- moduleManagers()$selectionManager$selectedUIDs()
        
        # flush cache?
        flushCache <- TRUE
        
        if (!is.null(resultFlushCache())) {
          # TODO is there a better way to do this?
          # ie/ if other images are selected but the
          # underlying data has not changed
          # do not flush the cache
          if (resultFlushCache() == moduleManagers()$imageViewerManager$imageUpdated()) {
            flushCache <- FALSE
          }
        }
        
        progress <- Progress$new()
        
        progress$set(
          message = sprintf("Get DTs for %d images (flush cache = %s)",
                            length(uIDs), flushCache),
          value = 50)
        
        counter <- 1
        # get population DT
        popDT(rbindlist(lapply(
          moduleManagers()$imageSetManager$selectedSet()$cciaObjects(uIDs = uIDs), 
          function(x) {
            rbindlist(
              lapply(
                resultParamsPops(),
                function(y) {
                  progress$set(
                    message = sprintf(
                      "%s (%d/%d)", x()$getUID(), counter, length(uIDs)),
                    value = 50)
                  
                  counter <<- counter + 1
                  
                  x()$popDT(
                    popType(), y, includeFiltered = TRUE,
                    flushCache = flushCache
                  )
                } 
              ), idcol = "pop", fill = TRUE
            )
          }), idcol = "uID", fill = TRUE)[expInfo(), on = 'uID']
        )
        
        # set cache flush
        resultFlushCache(moduleManagers()$imageViewerManager$imageUpdated())
        
        progress$close()
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
                cciaObj$popUtils(popType())$popPaths()
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
        cciaObj()$savePops(
          popType(), includeFiltered = TRUE, purge = TRUE)
      })
      
      # show hmm parameters
      observeEvent(resultParamsCols(), {
        req(resultParamsCols())
        req(resultParamsColName())
        
        # show plots
        for (x in resultParamsCols()) {
          local({
            local_x <- x
            
            output[[sprintf("hmmPlotParams_%s", local_x)]] <- renderPlot({
              req(popDT())
              req(expInfo())
              
              # plot properties
              p1 <- ggplot(
                popDT() %>%
                  pivot_longer(
                    cols = local_x,
                    names_to = "measure",
                    values_to = "value") %>%
                  drop_na(any_of(c("value", resultParamsColName()))) %>%
                  dplyr::mutate(across(c(resultParamsColName()), factor))
                , aes(x = get(resultParamsColName()), y = value,
                      fill = get(resultParamsColName()))) +
                theme_classic() +
                xlab("") +
                ylab(local_x)
              p1 %>% plotThemeViolin(angle = 0)
            })
          })
        }
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
          # behaviour is calculated on a set level
          # # moduleManagers()$taskManager$createTaskDataTableColumns() 
        ))
      })
      
      ## Plots
      output$tracksClustersPlots <- renderUI({
        # do the same as for region and pop clustering
        # OK
        
      })
      
      output$hmmPlots <- renderUI({
        req(resultParamsCols())
        
        # create parameter plots
        pParams <- list()
        for (x in resultParamsCols()) {
          pParams[[x]] <- column(6, plotOutput(
            session$ns(sprintf("hmmPlotParams_%s", x)),
            height = "300px", width = "100%"
          ))
        }
        
        # create summary plot
        pSummary <- plotOutput(
          session$ns("hmmPlotSummary"),
          height = "300px", width = "100%"
        )
        
        # return layout
        tagList(
          box(
            solidHeader = TRUE,
            collapsible = FALSE, 
            title = "Parameters",
            status = "primary",
            width = 6,
            pParams
          ),
          box(
            solidHeader = TRUE,
            collapsible = FALSE, 
            title = "Summary",
            status = "primary",
            width = 6,
            pSummary
          )
        )
      })
      
      output$hmmPlotSummary <- renderPlot({
        req(popDT())
        req(resultSummaryAxisX())
        req(resultParamsColName())
        
        # make summary
        # TODO data.table only
        summaryDF <- popDT()[, .(n.state = .N),
                             by = eval(c("pop", "uID", resultParamsColName()))] %>%
          drop_na() %>% 
          group_by(pop, uID) %>%
          dplyr::mutate(freq.state = n.state/sum(n.state)) %>%
          left_join(expInfo())
        
        # plot
        p1 <- ggplot(summaryDF,
               aes(
                 if (resultSummaryInteraction() != "NONE")
                   interaction(
                     get(resultSummaryAxisX()),
                     get(resultSummaryInteraction())
                   )
                 else
                   as.factor(get(resultSummaryAxisX())),
                 freq.state,
                 fill = as.factor(get(resultParamsColName())))
               ) +
          ylab("Frequency type") + xlab("") +
          ylim(0, 1)
        
        p1 %>% plotThemeBoxplot()
      })
      
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
      output$resultParams <- renderUI({
        req(cciaObj())
        
        # get live columns
        liveCols <- cciaObj()$labelPropsCols(popType = "live")
        
        # create ui elements
        tagList(fluidRow(
          column(
            3,
            tags$label("Parameter plots"),
            createSelectInput(
              session$ns("resultParamsPops"),
              label = "Populations",
              choices = unname(cciaObj()$popPaths(
                popType(), includeFiltered = TRUE)),
              multiple = TRUE,
              selected = resultParamsPops()
            ),
            createSelectInput(
              session$ns("resultParamsCols"),
              label = "Properties",
              choices = unname(cciaObj()$labelPropsCols()),
              multiple = TRUE,
              selected = shinyInputValue(
                "resultParamsCols", input,
                c(
                  "live.cell.speed", "live.cell.angle",
                  "ellipticity", "area"
                ))
            ),
            createSelectInput(
              session$ns("resultParamsColName"),
              label = "HMM type",
              choices = liveCols[!is.na(
                stringr::str_match(liveCols, "^live\\.cell\\.hmm\\.state\\..*"))],
              multiple = FALSE,
              selected = resultParamsColName()
            )
          ),
          column(
            3,
            tags$label("Summary plots"),
            createSelectInput(
              session$ns("resultSummaryAxisX"),
              label = "X Axis",
              choices = colnames(expInfo()),
              multiple = FALSE,
              selected = resultSummaryAxisX()
            ),
            createSelectInput(
              session$ns("resultSummaryInteraction"),
              label = "Interaction",
              choices = c("NONE", colnames(expInfo())),
              multiple = FALSE,
              selected = resultSummaryInteraction()
            )
            # createSelectInput(
            #   session$ns("resultSummaryFill"),
            #   label = "X Axis",
            #   choices = list(
            #     "Filtered populations" = "pop",
            #     "Base populations" = "valueName",
            #     "HMM states" = "live.cell.hmm.state"
            #   ),
            #   multiple = FALSE,
            #   selected = input$resultSummaryFill
            # ),
          )
        ))
      })
      
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
          napariModule = "behaviour_analysis"
        ),
        task = list(
          funLabel = "Behaviour analysis method",
          taskVarsToAdd = taskVarsToAdd,
          runTaskCombinedSelect = TRUE
        ),
        population = list(
          popData = popData,
          popType = popType,
          updateImage = updateImage,
          enableFilterPopulation = TRUE
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
