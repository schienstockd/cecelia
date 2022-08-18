# UI manage image sets
populationUI <- function(
  id, enableAddPopulation = FALSE, enableFilterPopulation = FALSE,
  createFilterPopulations = TRUE, boxWidth = 12) {
  # every file starts with the namespace
  ns <- NS(id)
  
  # add population elements
  addPopUI <- list()
  if (enableAddPopulation == TRUE) {
    addPopUI <- list(
      tags$hr(),
      fluidRow(
        column(
          2,
          textInput(ns("popName"), "Name")
        ),
        column(
          2,
          uiOutput(ns("popParent"))
        ),
        column(
          2,
          actionButton(ns("addPop"), "Add Population")
        )
      )
    )
  }
  
  # create filter box
  filterPopUI <- list()
  if (all(enableFilterPopulation, createFilterPopulations)) {
    filterPopUI <- box(
      solidHeader = TRUE,
      collapsible = TRUE, 
      # collapsed = TRUE,
      collapsed = FALSE,
      title = "Create Filter Populations",
      status = "primary",
      width = boxWidth,
      fluidRow(
        column(
          12,
          textInput(ns("filteredPopName"), "New name"),
          uiOutput(ns("filteredPopParents")),
          sliderInput(ns("filteredPopNumMeasures"), "Number of filters", 1, 12, 1),
          fluidRow(
            column(6, tags$label("Measure")),
            column(3, tags$label("Function")),
            column(3, tags$label("Values"))
          ),
          fluidRow(
            column(6, uiOutput(ns("filteredPopMeasure"))),
            column(3, uiOutput(ns("filteredPopFun"))),
            column(3, uiOutput(ns("filteredPopValues")))
          ),
          checkboxInput(ns("filterDefaultAll"), "Show all by default", value = FALSE),
          checkboxInput(ns("isTrack"), "Is Track", value = FALSE),
          actionButton(ns("addFilteredPop"), "Add")
        )
      )
    )
  }
  
  # create box
  popBox <- box(
    solidHeader = TRUE,
    collapsible = TRUE, 
    title = "Define Populations",
    status = "primary",
    width = boxWidth,
    fluidRow(
      column(
        12,
        DT::dataTableOutput(ns("popTable")),
        uiOutput(ns("popPointsSize")),
        # actionButton(ns("loadPopMapping"), "Load"),
        actionButton(ns("savePopMapping"), "Save"),
        actionButton(ns("propagatePopMapping"), "Propagate to Selected")
      )
    ),
    # add populations
    addPopUI
  )
  
  # return
  do.call(tagList, list(popBox, filterPopUI))
}
