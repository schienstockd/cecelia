#' @description UI manage image set attributes
#' @param id character of module ID
#' @examples
#' TODO
.imageSetAttributesUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  tagList(
    fluidRow(column(
      12,
      tags$label("Filters"),
         fluidRow(
           column(
             4, shinyjs::disabled(actionButton(ns("attrFilterApply"), "Apply"))),
           column(
             4, shinyjs::disabled(actionButton(ns("attrFilterReset"), "Reset"))),
           column(
             4, shinyjs::disabled(checkboxInput(ns("attrFilterInvert"), "Invert")))
           )
         )),
    fluidRow(uiOutput(ns("attrFilter")))
  )
}
