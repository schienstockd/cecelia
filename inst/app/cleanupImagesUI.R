#' @description UI for cleaning up images
#' @param id character of module ID
#' @examples
#' TODO
.cleanupImagesUI <- function(id) {
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
            title = "Cleanup parameters",
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
        )
      )
    )
  )
}
