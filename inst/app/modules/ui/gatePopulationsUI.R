#' @description UI to gate populations
#' @param id character of module ID
#' @examples
#' TODO
.gatePopulationsUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  tagList(
    useKeys(),
    keysInput(ns("selectionHotkeys"), selectionHotkeys),
    
    fluidRow(
      column(
        9,
        fluidRow(
          box(
            id = ns("imageTableBox"),
            solidHeader = TRUE,
            collapsible = TRUE, 
            title = "Select Images",
            status = "primary",
            width = 12,
            fluidRow(column(3, .imageSetUI(id))),
            fluidRow(
              column(2, .imageSetAttributesUI(id)),
              column(
                10,
                DT::dataTableOutput(ns("imageTable"))
              )
            )
          )
        ),
        .flowPlotUI(id, enableGatePopulation = TRUE)
      ),
      column(
        3,
        fluidRow(
          .taskManagerUI(id)
        ),
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = TRUE, 
            title = "Gating parameters",
            status = "primary",
            width = 12,
            fluidRow(
              column(
                12,
                fluidRow(
                  column(
                    6,
                    sliderInput(ns("numFlowPlots"), "Plots", 2, 10, 
                                cciaConf()$fcs$gating$plots$num),
                    checkboxInput(ns("useFlowColours"), "Density plot", TRUE),
                    sliderInput(ns("plotlyScreenshotHeight"), "Screenshot size", 100, 1000, 400, 25),
                    sliderInput(ns("markerOpacity"), "Marker Opacity", 0.05, 1, 0.2, 0.05)
                  ),
                  column(
                    6,
                    tags$label("Image"), tags$br(),
                    actionButton(ns("updateImage"), "Update Image"),
                    checkboxInput(ns("autoUpdateImage"), "Auto Update"),
                    actionButton(ns("resetImageLabelSelection"), "Reset selection"),
                    tags$br(), tags$br(),
                    actionButton(ns("debugPlotsRendered"), "Plots rendered")
                  )
                ),
                tags$hr(),
                uiOutput(ns("taskFunction"))
              )
            ),
            uiOutput(ns("funParams"))
          )
        ),
        fluidRow(
          populationUI(id)
        ),
        fluidRow(
          .shapesUI(id)
        )
      )
    )
  )
}
