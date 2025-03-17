renv::load("~/R-workspace/cecelia/")

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

library(flowCore)
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
devtools::load_all("../../")
# cciaUse("~/cecelia/dev")
cciaUse("~/cecelia/dev", initConda = TRUE, initJupyter = TRUE)
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
# pID <- "Co3HDh" # 2P
pID <- "7cDkr2" # Cornea
versionID <- 1

# init ccia object
cciaObj.TEST <- initCciaObject(
  # pID = pID, uID = "lWinrY", versionID = versionID, initReactivity = FALSE # Tcells
  pID = pID, uID = "MZen4N", versionID = versionID, initReactivity = FALSE # Tcells
)

# init populations
popsList <- cciaObj.TEST$popPaths(popType = "live", includeFiltered = TRUE)
popsTracked <- popsList[!is.na(stringr::str_match(popsList, "/tracked$"))]

# popDT <- cciaObj.TEST$popDT("live", pops = c(popsTracked[[1]]), includeFiltered = TRUE)
rootDT.TEST <- cciaObj.TEST$popDT("live", pops = c(.flowPopParent(popsTracked[[1]]), popsTracked[[1]]), includeFiltered = TRUE)
colPal <- randomcoloR::distinctColorPalette(length(unique(rootDT.TEST$track_id)))

# convert to physical values
rootDT.TEST <- convertPixelToPhysical(rootDT.TEST, cciaObj.TEST$omeXMLPixelRes(invalidate = FALSE))

# get graph
# g <- cciaObj.TEST$tracksGraph(
#   completeDT = TRUE,
#   replaceNA = TRUE,
#   pop = popsTracked[[1]],
#   extraAttr = c("label")
# )

# add graph information
# set.seed(1984)
# L.DT <- as.data.table(igraph::layout_as_tree(g))
# colnames(L.DT) <- c("L1", "L2")
# L.DT[, label := igraph::get.vertex.attribute(g, "label")]
# popDT[L.DT, on = c("label"), `:=` (L1 = L1, L2 = L2)]

id <- "celltrackViewer"
ns <- NS(id)

# init napari
viewer <- NapariUtils$new()
viewer$initNapari()

# init settings
valueName.TEST <- .flowPopParent(popsTracked[[1]])
viewer$setTaskDir(cciaObj.TEST$persistentObjectDirectory())
imChannelNames <- cciaObj.TEST$imChannelNames()

# get time information
showTimestamp <- cciaObj.TEST$omeXMLPixels()$SizeT > 1

if (showTimestamp == TRUE) {
  timeInterval <- cciaObj.TEST$omeXMLTimelapseInfo()$interval
} else {
  timeInterval <- 1
}

# make sure that the DT order matches the labels order
# this can happen when you manually add segmentation?
labels <- cciaObj.TEST$labelProps(valueName = valueName.TEST)
rootDT.TEST[, label := factor(label, levels = labels$values_obs()$label)]
setorder(rootDT.TEST, label)
labels$close()

# open image
viewer$setTaskDir(cciaObj.TEST$persistentObjectDirectory())
viewer$openImage(
  cciaObj.TEST$imFilepath(valueName = NULL),
  imChannelNames = unname(imChannelNames),
  show3D = FALSE,
  showTimestamp = showTimestamp,
  timeInterval = timeInterval,
  napariModule = "tracking_correction",
  layersVisible = FALSE
)

# show labels
viewer$showLabelsAll(
  list(valueName.TEST),
  showPoints = TRUE,
  showTracks = TRUE
)

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
      column(
        12,
        fluidRow(
          column(4, DT::dataTableOutput(ns("pointsTable"))),
          column(3, DT::dataTableOutput(ns("tracksTable"))),
          column(2,
                 tags$h5("Points operations"),
                 fluidRow(uiOutput(ns("pointsOps"))),
                 tags$br(),
                 tags$h5("Tracks operations"),
                 fluidRow(uiOutput(ns("tracksOps")))
          ),
          column(3, DT::dataTableOutput(ns("editHistory")))
        ),
      ),
      column(
        12,
        fluidRow(
          column(4, plotlyOutput(ns("pointsPreview"), height = "400px", width = "100%")),
          column(4, plotlyOutput(ns("tracksPreview"), height = "400px", width = "100%")),
          column(4, plotlyOutput(ns("trackTraces"), height = "400px", width = "100%"))
        ),
      )
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
      # record track edits
      recordTrackEdits <- function(x, ...) {
        # record changes
        tracksEditHistory(
          append(tracksEditHistory(), list(track.diffs(tracksCurrentIDs(), x, ...))))
        
        # set new track ids
        tracksCurrentIDs(x)
      }
      
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
      
      # ccia object
      cciaObj <- reactive({
        cciaObj.TEST
      })
      
      # get root
      # !! THIS IS DEBUG ONLY !!
      rootDT <- eventReactive(updatePopDT(), {
        # record track ids
        # copy here otherwise the sequence gets changed whenever DT changes
        # !! reactive violation !!
        tracksCurrentIDs(copy(rootDT.TEST$track_id))
        
        rootDT.TEST
      })
      
      valueName <- reactive({
        valueName.TEST
      })
      
      # dependent on rootDT
      tracksDT <- eventReactive(rootDT(), {
        req(rootDT())
        
        rootDT()[track_id > 0]
      })
      
      # return tracking history as DF
      tracksEditHistoryDF <- reactive({
        # get data
        DT <- as.data.table(list(Function = sapply(tracksEditHistory(), function(x) x$short)))
        nEdits <- nrow(DT)
        modCols <- list()
        
        # prepare modifier columns
        if (nEdits > 0) {
          editIDs <- seq(nEdits)
          modCols <- list(
            "Rollback" = shinyInput(
              "actionLink", session$ns("trackEditRollback_"), editIDs,
              initIcons = rep(btnICON_POINT_LEFT, nEdits),
              initOnclick = paste(
                sprintf(
                  'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
                  session$ns("trackEditRollback"),
                  seq(editIDs)
                )
              )
            )
          )
        }
        
        # bind
        cbind(DT, do.call(cbind, modCols))
      })
      
      # reactives for storing point/track information
      # this information will come from napari or other
      storeLabels <- reactiveVal()
      storePointsDT <- reactiveVal()
      storePoints <- reactiveVal()
      storeTracks <- reactiveVal()
      tracksModifies <- reactiveVal()
      
      # helpers for modification
      updatePopDT <- reactiveVal(1)
      tracksEditHistory <- reactiveVal()
      tracksCurrentIDs <- reactiveVal()
      
      # get selected cells
      observeEvent(viewerOuput(), {
        req(viewerOuput())
        
        req(length(viewerOuput()$trackingCorrectionSelectPoints) > 0)
        
        # save points
        storeLabels(viewerOuput()$trackingCorrectionSelectPoints)
      })
      
      # points depends on selected labels
      observeEvent(c(storeLabels(), rootDT()), {
        req(length(storeLabels()) > 0)
        
        # get points DT
        # DT <- copy(rootDT()[label %in% storeLabels()])
        track.ids <- rootDT()[label %in% storeLabels()]$track_id
        DT <- copy(rootDT()[track_id %in% track.ids])
        
        storePointsDT(DT)
        
        # storeTracks(DT)
        # as.data.frame otherwise DT will be unhappy
        storePoints(as.data.frame(DT[
          label %in% storeLabels(), c("track_id", "label", "centroid_t")] %>% rename(uID = label)))
      })
      
      # tracks depends on selected points
      observeEvent(storePoints(), {
        req(nrow(storePoints()) > 0)
        
        storeTracks(unique(storePoints()[, c("track_id"), drop = FALSE]) %>% rename(uID = track_id))
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
      
      # listen to track modification
      observeEvent(c(
        updatePopDT()
      ), {
        req(updatePopDT())
        req(rootDT())
        req(tracksDT())
        
        # save tracks
        tracks.save.mod(cciaObj(), rootDT(), valueName = valueName())
        
        # call napari
        # TODO this is different
        # globalManagers$viewerManager()$viewer()$highlightTracks(
        #   paste0(valueName(), "-mod"), popTracksFiltered()$track_id, "filtered")
        # globalManagers$viewerManager()$viewer()$showLabelsAll(
        #   list(paste0(valueName(), "-mod")), showTracks = TRUE)
        
        viewer$highlightTracks(
          paste0(valueName(), "-mod"), tracksDT()$track_id, "selected")
        viewer$showLabelsAll(
          list(paste0(valueName(), "-mod")), showTracks = TRUE)
      })
      
      # Listen to points/tracks OPs
      observeEvent(input$pointsOpRm, {
        req(length(pointsSelection$selectedUIDs()) > 0)
        
        # get points for operation
        tracks.points.rm(rootDT(), pointsSelection$selectedUIDs())
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("PT.rm", paste(pointsSelection$selectedUIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$pointsOpAdd, {
        req(length(pointsSelection$selectedUIDs()) > 0)
        
        # get points for operation
        tracks.points.add(rootDT(), pointsSelection$selectedUIDs(),
                          trackID = tracksSelection$selectedUIDs())
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("PT.add", paste(pointsSelection$selectedUIDs(), collapse = ","),
                        "to", paste(tracksSelection$selectedUIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$pointsOpSave, {
        # save tracking correction
        # copy modified version over to original tracking
        labelsPath <- cciaObj()$imLabelPropsFilepath(valueName())
        modPath <- paste0(
          tools::file_path_sans_ext(labelsPath), "-mod.", tools::file_ext(labelsPath))

        file.copy(modPath, labelsPath, overwrite = TRUE)
      })
      
      observeEvent(input$tracksOpRm, {
        req(length(tracksSelection$selectedUIDs()) > 0)
        
        # get points for operation
        tracks.rm(rootDT(), tracksSelection$selectedUIDs())
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("TK.rm", paste(tracksSelection$selectedUIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$tracksOpJoin, {
        req(length(tracksSelection$selectedUIDs()) > 0)
        
        # get points for operation
        tracks.join(rootDT(), tracksSelection$selectedUIDs()[[1]], tracksSelection$selectedUIDs()[[2]])
        
        # record
        recordTrackEdits(
          rootDT()$track_id,
          short = paste("TK.join", paste(tracksSelection$selectedUIDs(), collapse = ",")))
        
        # update pop DT
        updatePopDT(runif(1))
      })
      
      observeEvent(input$edtTracksSave, {
        # save tracking correction
        # copy modified version over to original tracking
        labelsPath <- cciaObj()$imLabelPropsFilepath(valueName())
        modPath <- paste0(
          tools::file_path_sans_ext(labelsPath), "-mod.", tools::file_ext(labelsPath))
        
        file.copy(modPath, labelsPath, overwrite = TRUE)
      })
      
      # rollback changes
      observeEvent(input$trackEditRollback, {
        req(cciaObj())
        
        
        
        # update track IDs
        rollback.result <- track.edits.rollback(
          tracksCurrentIDs(), tracksEditHistory(), as.numeric(input$trackEditRollback))
        
        # update DT and ids
        rootDT()[, track_id := rollback.result$x]
        tracksCurrentIDs(rollback.result$x)
        
        # update history
        tracksEditHistory(rollback.result$edit.history)
        
        updatePopDT(runif(1))
      })
      
      ### UI Outputs
      # output$plotData <- renderText({
      #   suppressWarnings(
      #     paste(
      #       paste("plotly_hover", paste(event_data("plotly_hover", "trackTraces"), collapse = "; ")),
      #       paste("plotly_unhover", paste(event_data("plotly_unhover", "trackTraces"), collapse = "; ")),
      #       paste("plotly_click", paste(event_data("plotly_click", "trackTraces"), collapse = "; ")),
      #       paste("plotly_doubleclick", paste(event_data("plotly_doubleclick", "trackTraces"), collapse = "; ")),
      #       paste("plotly_selected", paste(event_data("plotly_selected", "trackTraces"), collapse = "; ")),
      #       paste("plotly_selecting", paste(event_data("plotly_selecting", "trackTraces"), collapse = "; ")),
      #       paste("plotly_brushed", paste(event_data("plotly_brushed", "trackTraces"), collapse = "; ")),
      #       paste("plotly_brushing", paste(event_data("plotly_brushing", "trackTraces"), collapse = "; ")),
      #       paste("plotly_deselect", paste(event_data("plotly_deselect", "trackTraces"), collapse = "; ")),
      #       paste("plotly_relayout", paste(event_data("plotly_relayout", "trackTraces"), collapse = "; ")),
      #       paste("plotly_restyle", paste(event_data("plotly_restyle", "trackTraces"), collapse = "; ")),
      #       paste("plotly_legendclick", paste(event_data("plotly_legendclick", "trackTraces"), collapse = "; ")),
      #       paste("plotly_legenddoubleclick", paste(event_data("plotly_legenddoubleclick", "trackTraces"), collapse = "; ")),
      #       paste("plotly_clickannotation", paste(event_data("plotly_clickannotation", "trackTraces"), collapse = "; ")),
      #       paste("plotly_afterplot", paste(event_data("plotly_afterplot", "trackTraces"), collapse = "; ")),
      #       paste("plotly_sunburstclick", paste(event_data("plotly_sunburstclick", "trackTraces"), collapse = "; ")),
      #       paste("plotly_sliderchange", paste(input$plotly_sliderchange, collapse = "; ")),
      #       sep = "\r\n"
      #     )
      #   )
      # })
      
      ## Tables
      # points data
      output$pointsTable <- DT::renderDataTable({
        req(storePoints())
        req(nrow(storePoints()) > 0)
        
        # add scollbars to table
        # https://stackoverflow.com/a/73221455
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          pointsSelection$createSelectionColumn(),
          storePoints()
          # moduleManagers()$taskManager$createTaskDataTableColumns()
        ), pageLength = 6, dom = "tip")
      })
      
      # tracks data
      output$tracksTable <- DT::renderDataTable({
        req(storeTracks())
        req(nrow(storeTracks()) > 0)
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          tracksSelection$createSelectionColumn(),
          storeTracks()
          # moduleManagers()$taskManager$createTaskDataTableColumns()
        ), pageLength = 6, dom = "tip")
      })
      
      # Edit history
      output$editHistory <- DT::renderDataTable({
        req(tracksEditHistoryDF())
        
        # get table
        options = list(
          fixedColumns = list(leftColumns = 1),
          fixedHeader = TRUE
          )
        
        tableOpts <- list(
          ordering = TRUE,
          dom = "tip",
          pageLength = 8
        )
        
        moduleManagers()$uiManager$dataTable(
          tracksEditHistoryDF(), options = options, rownames = TRUE, editable = FALSE,
          ordering = tableOpts$ordering, pageLength = tableOpts$pageLength, dom = tableOpts$dom)
      }, server = TRUE)
      
      # update table without triggering redraw
      observeEvent(c(
        tracksEditHistoryDF()
        # updatePopTable()
      ), {
        req(tracksEditHistoryDF())
        
        # https://stackoverflow.com/a/56879871/13766165
        replaceData(dataTableProxy("editHistory"),
                    tracksEditHistoryDF(),
                    resetPaging = FALSE, rownames = FALSE)
      })
      
      # Plots
      output$trackTraces <- renderPlotly({
        req(storePointsDT())
        
        plot_ly(source = "trackTraces") %>%
          add_trace(
            data = storePointsDT()[track_id > 0],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE) %>%
          layout(
            xaxis = list(scaleanchor = "y", scaleratio = 1, range = c(0, max(rootDT()$centroid_x))),
            yaxis = list(autorange = "reversed", range = c(0, max(rootDT()$centroid_y)))
          ) %>%
          toWebGL()
      })
      
      output$pointsPreview <- renderPlotly({
        # req(pointsSelection$selectedUIDs())
        
        plot_ly(source = "pointsPreview") %>%
          add_trace(
            data = storePointsDT()[track_id > 0 & track_id],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE,
            marker = list(color = "lightgrey"), line = list(color = "lightgrey")) %>%
          add_trace(
            data = storePointsDT()[track_id > 0 & label %in% pointsSelection$selectedUIDs()],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "markers", showlegend = FALSE) %>%
          add_trace(
            data = storePointsDT()[is.na(track_id) & label %in% pointsSelection$selectedUIDs()],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "markers", showlegend = FALSE,
            marker = list(color = "#ff1493")) %>%
          layout(
            xaxis = list(scaleanchor = "y", scaleratio = 1, range = c(0, max(rootDT()$centroid_x))),
            yaxis = list(autorange = "reversed", range = c(0, max(rootDT()$centroid_y)))
          ) %>%
          toWebGL()
      })
      
      output$tracksPreview <- renderPlotly({
        # req(tracksSelection$selectedUIDs())
        
        plot_ly(source = "tracksPreview") %>%
          add_trace(
            data = storePointsDT()[track_id > 0 & track_id],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE,
            marker = list(color = "lightgrey"), line = list(color = "lightgrey")) %>%
          add_trace(
            data = storePointsDT()[track_id %in% tracksSelection$selectedUIDs()],
            # data = rootDT(),
            x = ~centroid_x, y = ~centroid_y, split = ~track_id,
            type = "scatter", mode = "lines+markers", showlegend = FALSE) %>%
          layout(
            xaxis = list(scaleanchor = "y", scaleratio = 1, range = c(0, max(rootDT()$centroid_x))),
            yaxis = list(autorange = "reversed", range = c(0, max(rootDT()$centroid_y)))
          ) %>%
          toWebGL()
      })
      
      ## Other
      output$pointsOps <- renderUI({
        fluidRow(
          actionButton(session$ns("pointsOpRm"), "Remove"),
          actionButton(session$ns("pointsOpAdd"), "Add"),
          actionButton(session$ns("pointsOpSave"), "save")
        )
      })
      
      output$tracksOps <- renderUI({
        fluidRow(
          actionButton(session$ns("tracksOpRm"), "Remove"),
          actionButton(session$ns("tracksOpJoin"), "Join")
        )
      })
      
      ## DEBUG
      managerNames = c("ui", "input")
      managerConf = list(
        moduleName = id,
        cciaObj = cciaObj
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, NULL, id, managerNames, managerConf)
      
      # selection managers
      pointsSelection <- createSelectionManager(
        input, output, session, globalManagers, moduleManagers,
        list(selectionData = storePoints, selectionID = "points")
      )
      tracksSelection <- createSelectionManager(
        input, output, session, globalManagers, moduleManagers,
        list(selectionData = storeTracks, selectionID = "tracks")
      )
    }
  )
}

shinyApp(ui = ui, server = server)
