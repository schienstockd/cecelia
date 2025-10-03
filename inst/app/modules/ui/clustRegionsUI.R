#' @description UI to clust populations
#' @importFrom plotly plotlyOutput
#' 
#' @param id character of module ID
#' @examples
#' TODO
.clustRegionsUI <- function(id) {
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
          ),
          fluidRow(
            column(
              12,
              box(
                solidHeader = TRUE,
                collapsible = TRUE, 
                title = "Region maps",
                status = "primary",
                width = 12,
                fluidRow(
                  column(
                    12,
                    tabsetPanel(
                      id = ns("heatmapType"),
                      type = "tabs",
                      tabPanel(
                        "Heatmap", value = "heatmap",
                        plotlyOutput(ns("heatmapPlot"), height = "600px")),
                      tabPanel(
                        "Correlation map", value = "correlation",
                        plotlyOutput(ns("correlationPlot"), height = "600px"))
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              12,
              populationUI(id, boxWidth = 12, enableAddPopulation = TRUE)
            )
          )
        )
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
            title = "Mapping parameters",
            status = "primary",
            width = 12,
            fluidRow(
              column(
                12,
                tags$label("Image"), tags$br(),
                actionButton(ns("updateImage"), "Update Image"),
                checkboxInput(ns("autoUpdateImage"), "Auto Update"),
                actionButton(ns("resetImageLabelSelection"), "Reset selection"),
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
        )
      )
    )
  )
}
