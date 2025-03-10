renv::load("~/R-workspace/cecelia/")

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

library(flowCore)
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
devtools::load_all("../../")
cciaUse("~/cecelia/dev")
# cciaUse("~/cecelia/dev", initConda = TRUE, initJupyter = TRUE)
# cciaUse("~/cecelia/dev", initConda = FALSE)

# source all files from subdirectories
# TODO is there a better way of doing this?
appSources <- c(
  file.path("..", "inst", "app", "constantsCore.R"),
  list.files(file.path("..", "inst", "app", "lib"), pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("..", "inst", "app", "helpers"), pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("..", "inst", "app", "modules", "managers"), pattern = ".R$", recursive = TRUE, full.names = TRUE)
)

for (x in appSources) {
  source(x)
}

# set test variables
pID <- "Co3HDh"
versionID <- 1

# init ccia object
cciaObj <- initCciaObject(
  pID = pID, uID = "lWinrY", versionID = versionID, initReactivity = FALSE # Tcells
)

# init populations
popsList <- cciaObj$popPaths(popType = "live", includeFiltered = TRUE)
popsTracked <- popsList[!is.na(stringr::str_match(popsList, "/tracked$"))]

popDT <- cciaObj$popDT("live", pops = c(popsTracked[[1]]), includeFiltered = TRUE)
colPal <- randomcoloR::distinctColorPalette(length(unique(popDT$track_id)))

# get graph
g <- cciaObj$tracksGraph(
  completeDT = TRUE,
  replaceNA = TRUE,
  pop = popsTracked[[1]],
  extraAttr = c("label")
)

# add graph information
set.seed(1984)
L.DT <- as.data.table(igraph::layout_as_tree(g))
colnames(L.DT) <- c("L1", "L2")
L.DT[, label := igraph::get.vertex.attribute(g, "label")]
popDT[L.DT, on = c("label"), `:=` (L1 = L1, L2 = L2)]

id <- "celltrackViewer"
ns <- NS(id)

# # init napari
# viewer <- NapariUtils$new()
# viewer$initNapari()
# 
# # init settings
# valueName <- .flowPopParent(popsTracked[[1]])
# viewer$setTaskDir(cciaObj$persistentObjectDirectory())
# imChannelNames <- cciaObj$imChannelNames()
# 
# # get time information
# showTimestamp <- cciaObj$omeXMLPixels()$SizeT > 1
# 
# if (showTimestamp == TRUE) {
#   timeInterval <- cciaObj$omeXMLTimelapseInfo()$interval
# } else {
#   timeInterval <- 1
# }
# 
# # open image
# viewer$openImage(
#   cciaObj$imFilepath(valueName = NULL),
#   imChannelNames = unname(imChannelNames),
#   show3D = TRUE,
#   showTimestamp = showTimestamp,
#   timeInterval = timeInterval
# )
# 
# # show labels
# viewer$showLabelsAll(
#   popsTracked[[1]],
#   showPoints = TRUE,
#   showTracks = TRUE
# )

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    shinydashboard::box(
      id = ns("plotData"),
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Data selection",
      status = "primary",
      width = 12,
      verbatimTextOutput(ns("plotData"))
    )
  ),
  fluidRow(
    shinydashboard::box(
      id = ns("plots"),
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Plot output",
      status = "primary",
      width = 12,
      column(9, DT::dataTableOutput(ns("tracksTable"))),
      # column(9, uiOutput(ns("plotOutput"))),
      # column(9, sliderInput(ns("plotly_changeslider"), "Time", min = 0, max = 79, value = 0)),
    )
  ),
  
  # # --- JS ---
  # tags$head(includeScript(
  #   # file.path(cciaConf()$wwwDirs$pathToJS, "app.js"))),
  #   paste0(cciaConf()$wwwDirs$pathToJS, .Platform$file.sep, "app.js"))),
  
  # --- CSS ---
  tags$head(
    tags$link(
      rel = "stylesheet", type = "text/css",
      # href = file.path(cciaConf()$wwwDirs$pathToCss, "dark_mode.css"))
      href = paste0(cciaConf()$wwwDirs$pathToCss, .Platform$file.sep, "dark_mode.css"))
  )
)

server <- function(input, output, session) {
  moduleServer(
    id,
    function(input, output, session) {
      ### Functions
      
      # file listener to update plots
      # listen to viewer input
      viewerOuput <- reactivePoll(
        cciaConf()$python$viewer$outputDelay, session,
        checkFunc = function() {
          outputFile <- file.path(
            cciaPath(), "app",
            cciaConf()$python$viewer$viewerPath,
            cciaConf()$python$viewer$outputFile
          )
          
          if (file.exists(outputFile)) {
            return(file.mtime(outputFile))
          } else {
            return("")
          }
        },
        valueFunc = function() {
          outputFile <- file.path(
            cciaPath(), "app",
            cciaConf()$python$viewer$viewerPath,
            cciaConf()$python$viewer$outputFile
          )
          
          retVal <- NULL
          
          if (file.exists(outputFile)) {
            if (file.info(outputFile)$size > 1) {
              retVal <- jsonlite::fromJSON(outputFile)
            }
          }
          
          retVal
        }
      )
      
      # reactives for storing point/track information
      # this information will come from napari or other
      storePoints <- reactiveVal()
      storeTracks <- reactiveVal()
      
      observeEvent(viewerOuput(), {
        req(viewerOuput())
        
        req(length(viewerOuput()$selectedLabelIDs) > 0)
        
        # save points
        storePoints(viewerOuput()$selectedLabelIDs)
      })
      
      # depends on selected points
      observeEvent(storePoints(), {
        req(length(storePoints()) > 0)
        
        # get track IDs for points
        DT <- copy(popDT[label %in% storePoints(), c("track_id", "label", "centroid_t")])
        
        # prepare for showing table
        setnames(DT, "label", "uID")
        
        # storeTracks(DT)
        # otherwise DT will be unhappy
        storeTracks(as.data.frame(DT))
      })
      
      # listen to time changes to update plotly
      # in reality this would come from napari
      observeEvent(input$plotly_changeslider, {
        # Overview to trigger animate with plotly::plotlyProxyInvoke
        # https://plotly.com/javascript/plotlyjs-function-reference/#plotlyanimate
        p1 <- plotly::plotlyProxy("trackTraces")
        
        # create arguments
        p1 %>% plotlyProxyInvoke(
          # "animate", label = input$plotly_changeslider, value = input$plotly_changeslider,
          "animate",
          # data or steps
          list(
            input$plotly_changeslider
          ),
          # animation attributes
          list(
            transition = list(duration = 0),
            frame = list(duration = 0, redraw = FALSE)
          ))
      })
      
      ### UI Outputs
      output$plotData <- renderText({
        suppressWarnings(
          paste(
            paste("plotly_hover", paste(event_data("plotly_hover", "trackTraces"), collapse = "; ")),
            paste("plotly_unhover", paste(event_data("plotly_unhover", "trackTraces"), collapse = "; ")),
            paste("plotly_click", paste(event_data("plotly_click", "trackTraces"), collapse = "; ")),
            paste("plotly_doubleclick", paste(event_data("plotly_doubleclick", "trackTraces"), collapse = "; ")),
            paste("plotly_selected", paste(event_data("plotly_selected", "trackTraces"), collapse = "; ")),
            paste("plotly_selecting", paste(event_data("plotly_selecting", "trackTraces"), collapse = "; ")),
            paste("plotly_brushed", paste(event_data("plotly_brushed", "trackTraces"), collapse = "; ")),
            paste("plotly_brushing", paste(event_data("plotly_brushing", "trackTraces"), collapse = "; ")),
            paste("plotly_deselect", paste(event_data("plotly_deselect", "trackTraces"), collapse = "; ")),
            paste("plotly_relayout", paste(event_data("plotly_relayout", "trackTraces"), collapse = "; ")),
            paste("plotly_restyle", paste(event_data("plotly_restyle", "trackTraces"), collapse = "; ")),
            paste("plotly_legendclick", paste(event_data("plotly_legendclick", "trackTraces"), collapse = "; ")),
            paste("plotly_legenddoubleclick", paste(event_data("plotly_legenddoubleclick", "trackTraces"), collapse = "; ")),
            paste("plotly_clickannotation", paste(event_data("plotly_clickannotation", "trackTraces"), collapse = "; ")),
            paste("plotly_afterplot", paste(event_data("plotly_afterplot", "trackTraces"), collapse = "; ")),
            paste("plotly_sunburstclick", paste(event_data("plotly_sunburstclick", "trackTraces"), collapse = "; ")),
            paste("plotly_sliderchange", paste(input$plotly_sliderchange, collapse = "; ")),
            sep = "\r\n"
          )
        )
      })
      
      ## Tables
      # tracks data
      output$tracksTable <- DT::renderDataTable({
        req(storeTracks())
        req(nrow(storeTracks()) > 0)
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          moduleManagers()$selectionManager$createSelectionColumn(),
          # somehow data.table is not rendering properly in this context
          storeTracks()
          # moduleManagers()$taskManager$createTaskDataTableColumns()
        ))
      })
      
      ## Plots
      output$trackTraces <- renderPlotly({
        # https://plotly-r.com/client-side-linking
        popKey <- highlight_key(popDT, ~track_id)
        # popKey <- highlight_key(popDT, ~label)
        
        p1 <- plot_ly(
          # popDT,
          popDT %>% group_by(track_id),
          x = ~centroid_x,
          y = ~centroid_y,
          # alpha = 0.4,
          # color = I("black"),
          ids = NULL,
          type = "scatter",
          # mode = "lines+markers",
          mode = "lines",
          source = "trackTraces",
          color = ~track_id,
          colors = colPal,
          line = list(width = 5),
          marker = list(size = 0)
          ) %>%
          add_trace(
            data = popKey,
            alpha = 1,
            color = I("red"),
            type = "scatter",
            mode = "markers",
            frame = ~centroid_t,
            line = list(width = 0),
            marker = list(size = 10),
            ids = ~label) %>%
          hide_legend()
        # p2 <- plot_ly(popDT, x = ~live.cell.speed, y = ~live.cell.angle, source = "trackProps") %>%
        # # p2 <- plot_ly(popDT, x = ~centroid_x, y = ~centroid_y) %>%
        #   add_markers(alpha = 0.1, color = I("black")) %>%
        #   add_markers(data = popKey, color = I("red"), frame = ~centroid_t, ids = ~track_id)
        
        p2 <- popDT %>%
          group_by(track_id) %>%
          plot_ly(
            x = ~L1,
            y = ~centroid_t,
            color = ~track_id,
            colors = colPal,
            type = "scatter",
            mode = "lines+markers",
            source = "trackTraces",
            line = list(color = "#000000", width = 1),
            marker = list(color = "lightblue", size = 5),
            showlegend = FALSE,
            ids = ~label
          ) %>%
          add_markers(
            data = popKey,
            x = ~L1,
            y = ~centroid_t,
            marker = list(color = "red", size = 10),
            frame = ~centroid_t,
            ids = ~label
          )
        
        p <- subplot(p1, p2, nrows = 1, widths = c(0.5, 0.5),
                     titleX = TRUE, titleY = TRUE) %>%
          hide_legend() %>%
          animation_opts(frame = 30, transition = 0, redraw = FALSE, mode = "immediate") %>%
          highlight(
            "plotly_selected", 
            color = "gold", 
            opacityDim = 1, 
            hoverinfo = "none"
          )
        
        # p1 %>%
        #   toWebGL()
        # p %>%
        p2 %>%
          # List of non-implemented events
          # https://github.com/plotly/plotly.R/issues/1282#issuecomment-479150738
            # plotly_animated
            # plotly_animatingframe
            # plotly_animationinterrupted
            # plotly_autosize
            # plotly_beforeexport
            # plotly_buttonclicked
            # plotly_sliderchange
            # plotly_sliderend
            # plotly_sliderstart
            # plotly_transitioning
            # plotly_transitioninterrupted
          # https://plotly-r.com/js-event-handlers
          htmlwidgets::onRender(sprintf('
            function(el) { 
              el.on("plotly_sliderchange", function(d) { 
                console.log("plotly_sliderchange: ", d); 
                //console.log("plotly_sliderchange: ", d.step._index); 
                // If you need every event call add {priority: "event"}
                Shiny.setInputValue(\"%s\", d.step._index);
              });
            }
          ', session$ns("plotly_sliderchange")))
      })
      
      output$plotOutput <- renderUI({
        tagList(
          fluidRow(plotlyOutput(ns("trackTraces"), height = "800px", width = "1200px"))
        )
      })
      
      ## Other
      
      ## DEBUG
      managerNames = c("ui", "input", "selection")
      managerConf = list(
        moduleName = id,
        imageData = storeTracks,
        cciaObj = cciaObj
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, NULL, id, managerNames, managerConf)
    }
  )
}

shinyApp(ui = ui, server = server)
