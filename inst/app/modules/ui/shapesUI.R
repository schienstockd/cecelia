#' @description UI to manage image sets
#' @param id character of module ID
#' @param boxWidth integer of box width
#' @examples
#' TODO
.shapesUI <- function(
  id, boxWidth = 12) {
  # every file starts with the namespace
  ns <- NS(id)
  
  # create box
  popBox <- box(
    solidHeader = TRUE,
    collapsible = TRUE, 
    title = "Shape regions",
    status = "primary",
    width = boxWidth,
    fluidRow(
      column(
        12,
        DT::dataTableOutput(ns("shapesTable")),
        actionButton(ns("saveShapes"), "Save shapes"),
      )
    )
  )
  
  # return
  popBox
}
