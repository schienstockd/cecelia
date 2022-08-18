#' @description UI for pixel classification
#' @param id character of module ID
#' @examples
#' TODO
.pixelClassificationUI <- function(id) {
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
            fluidRow(
              column(3, .imageSetUI(id))
              ),
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
          populationUI(id, boxWidth = 12, enableAddPopulation = TRUE)
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
            title = "Analysis parameters",
            status = "primary",
            width = 12,
            fluidRow(
              column(12,
                     actionButton(ns("openPixcl"), "Classify pixels"),
                     actionButton(ns("saveLabels"), "Save Labels"),
                     actionButton(ns("resetLabelsScale"), "Reset scale")
              )
            ),
            tags$br(),
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
        # fluidRow(
        #   populationUI(id, enableFilterPopulation = TRUE)
        # )
      )
    )
  )
}
