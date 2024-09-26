library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

library(flowCore)
Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
devtools::load_all("../../")
cciaUse("~/cecelia/dev")
# cciaUse("~/cecelia/dev", initConda = FALSE)

# source all files from subdirectories
# TODO is there a better way of doing this?
appSources <- c(
  file.path("..", "inst", "app", "constantsCore.R"),
  list.files(file.path("..", "inst", "app", "lib"), pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("..", "inst", "app", "helpers"), pattern = ".R$", recursive = TRUE, full.names = TRUE)
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

popsList <- cciaObj$popPaths(popType = "live", includeFiltered = TRUE)
popsTracked <- popsList[!is.na(stringr::str_match(popsList, "/tracked$"))]

popDT <- cciaObj$popDT("live", pops = c(popsTracked[[1]]), includeFiltered = TRUE)

# g <- cciaObj$tracksGraph(
#   completeDT = TRUE,
#   replaceNA = TRUE,
#   pop = popsTracked[[1]]
# )
# colPal <- randomcoloR::distinctColorPalette(length(unique(popDT$track_id)))

id <- "celltrackViewer"
ns <- NS(id)

ui <- fluidPage(
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
      column(9, uiOutput(ns("plotOutput")))
    )
  )
)

server <- function(input, output, session) {
  moduleServer(
    id,
    function(input, output, session) {
      ### Functions
      
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
            paste("plotly_animated", paste(event_data("plotly_animated", "trackTraces"), collapse = "; ")),
            sep = "\r\n"
          )
        )
      })
      
      ## Tables
      
      ## Plots
      output$trackTraces <- renderPlotly({
        p1 <- plot_ly(popDT, x = ~centroid_x, y = ~(-centroid_y), source = "trackTraces") %>%
          add_markers(color = ~track_id, frame = ~centroid_t, ids = ~track_id) %>%
          hide_legend() %>%
          animation_opts(frame = 30, transition = 0, redraw = FALSE)
        
        # p1 %>%
        #   toWebGL()
        p1
      })
      
      output$plotOutput <- renderUI({
        tagList(
          fluidRow(plotlyOutput(ns("trackTraces"), height = "1200px"))
        )
      })
      
      ## Other
    }
  )
}

shinyApp(ui = ui, server = server)
