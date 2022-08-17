#' @description UI to import flow
#' @param id character of module ID
#' @examples
#' TODO
.importFlowUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  ### Managers
  tagList(
    # useKeys(),
    # keysInput(ns("selectionHotkeys"), selectionHotkeys),
    
    fluidRow(
      column(
        9,
        box(
          solidHeader = TRUE,
          collapsible = FALSE, 
          title = "Import Flow",
          status = "primary",
          width = 12,
          fluidRow(
            column(
              8,
              .imageSetUI(id, enableAddition = TRUE, enableDeletion = TRUE)
            ),
            column(
              4,
              fluidRow(tags$label("Add flow to set")),
              fluidRow(
                shinyjs::disabled(shinyFilesButton(
                  ns("imagesToImport"), "Select files",
                  "Select files", multiple = TRUE))
              )
            )
          ),
          fluidRow(
            column(2, .imageSetAttributesUI(id)),
            column(
              10,
              dataTableOutput(ns("imageTable"))
            )
          )
        ),
      ),
      column(
        3,
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = TRUE, 
            title = "Import parameters",
            status = "primary",
            width = 12,
            fluidRow(
              column(
                12,
                uiOutput(ns("taskFunction"))
              )
            ),
            uiOutput(ns("funParams"))
          )
        ),
        fluidRow(
          .taskManagerUI(id)
        )
      )
    )
  )
}
