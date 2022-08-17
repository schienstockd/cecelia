#' @descriptionUI UI for spatial analysis
#' @param id character of module ID
#' @param boxWidth integer of box width
#' @examples
#' TODO
.spatialAnalysisUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  tagList(
    # useKeys(),
    # keysInput(ns("selectionHotkeys"), selectionHotkeys),
    
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
                dataTableOutput(ns("imageTable"))
              )
            )
          )
        ),
        .flowPlotUI(id, enableGatePopulation = FALSE)
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
            title = "Analysis parameters",
            status = "primary",
            width = 12,
            fluidRow(
              column(
                12,
                fluidRow(
                  column(
                    6,
                    sliderInput(ns("numFlowPlots"), "Plots", 2, 10, 2),
                  ),
                  column(
                    6,
                    tags$label("Image"), tags$br(),
                    actionButton(ns("resetImageLabelSelection"), "Reset selection"),
                    tags$br(), tags$br(),
                    actionButton(ns("debugPlotsRendered"), "Plots rendered")
                  )
                ),
                tags$hr(),
                uiOutput(ns("popType")),
                uiOutput(ns("taskFunction"))
              )
            ),
            uiOutput(ns("funParams"))
          )
        ),
        fluidRow(
          .shapesUI(id)
        ),
        fluidRow(
          populationUI(id, enableFilterPopulation = TRUE)
        )
      )
    )
  )
}
