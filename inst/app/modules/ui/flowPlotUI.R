#' @description UI manage image sets
#' @param id character of module ID
#' @examples
#' TODO
.flowPlotUI <- function(
  id, enableGatePopulation = FALSE, boxWidth = 12) {
  # every file starts with the namespace
  ns <- NS(id)
  
  fluidRow(
    uiOutput(ns("flowPlots"))
  )
}