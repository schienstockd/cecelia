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
              )
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
          populationUI(id)
        ),
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = TRUE, 
            title = "Tracking correction parameters",
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
