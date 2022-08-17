#' @descriptionUI UI for tracking images
#' @param id character of module ID
#' @examples
#' TODO
.trackingImagesUI <- function(id) {
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
        fluidRow(
          box(
            id = ns("trackProps"),
            solidHeader = TRUE,
            collapsible = TRUE, 
            collapsed = TRUE,
            title = "Tracks properties",
            status = "primary",
            width = 12,
            background = "black",
            tabsetPanel(
              id = ns("tracksPlotsPanel"),
              type = "tabs",
              selected = "current",
              tabPanel(
                "Current image", value = "current",
                tags$br(),
                uiOutput(ns("tracksPlotsCurrent"))
              ),
              tabPanel(
                "Selected images", value = "set",
                tags$br(),
                uiOutput(ns("tracksPlotsSet")),
                # selection panel for plot
                uiOutput(ns("tracksPlotsSetLayout"))
              ),
              tabPanel(
                "Summary", value = "summary",
                tags$br(),
                uiOutput(ns("tracksPlotsSummary"))
              )
            )
          )
        ),
        fluidRow(
          box(
            id = ns("cellProps"),
            solidHeader = TRUE,
            collapsible = TRUE, 
            title = "Cells properties",
            status = "primary",
            width = 12,
            .flowPlotUI(id, enableGatePopulation = FALSE)
          )
        )
      ),
      column(
        3,
        fluidRow(
          .taskManagerUI(id)
        ),
        fluidRow(
          populationUI(id)
        ),
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = TRUE, 
            title = "Tracking parameters",
            status = "primary",
            width = 12,
            fluidRow(
              column(
                12,
                fluidRow(
                  column(
                    6,
                    tags$label("Image"), tags$br(),
                    actionButton(ns("resetImageLabelSelection"), "Reset selection"),
                  )
                ),
                tags$hr(),
                uiOutput(ns("taskFunction"))
              )
            ),
            uiOutput(ns("funParams"))
          )
        )
      )
    )
  )
}
