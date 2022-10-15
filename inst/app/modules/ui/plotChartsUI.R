#' @description UI to create canvas plots
#' @param id character of module ID
#' @examples
#' TODO
.plotChartsUI <- function(id) {
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
          shinydashboard::box(
            id = ns("plotData"),
            solidHeader = TRUE,
            collapsible = TRUE, 
            title = "Data selection",
            status = "primary",
            width = 12,
            uiOutput(ns("plotData"))
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
            column(3, uiOutput(ns("plotParams"))),
            column(9, uiOutput(ns("plotOutput")))
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
            title = "Chart parameters",
            status = "primary",
            width = 12,
            fluidRow(
              column(
                12,
                uiOutput(ns("popType")),
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
