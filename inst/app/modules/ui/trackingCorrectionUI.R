#' @descriptionUI UI for tracking correction
#' @param id character of module ID
#' @examples
#' TODO
.trackingCorrectionUI <- function(id) {
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
        fluidRow(
          box(
            id = ns("resultParams"),
            solidHeader = TRUE,
            collapsible = FALSE, 
            title = "Data Selection",
            status = "primary",
            width = 12,
            uiOutput(ns("resultParams"))
          )
        ),
        fluidRow(
          box(
            id = ns("qcPlots"),
            solidHeader = TRUE,
            collapsible = FALSE, 
            title = "Quality control plots",
            status = "primary",
            width = 8,
            uiOutput(ns("qcPlots"))
          ),
          box(
            id = ns("editHistory"),
            solidHeader = TRUE,
            collapsible = FALSE, 
            title = "Edit history",
            status = "primary",
            width = 4,
            # uiOutput(ns("editHistory"))
            DT::dataTableOutput(ns("editHistory"))
          )
        ),
        fluidRow(
          box(
            id = ns("trackMod"),
            solidHeader = TRUE,
            collapsible = FALSE, 
            title = "Track modification",
            status = "primary",
            width = 12,
            column(4, uiOutput(ns("qcTracksOverview"))),
            column(4, uiOutput(ns("qcTracksSelection"))),
            column(4, uiOutput(ns("qcTracksSelectionTable")))
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
