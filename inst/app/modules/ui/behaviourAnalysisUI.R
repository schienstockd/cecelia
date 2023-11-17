#' @description UI for behaviour analysis
#' @param id character of module ID
#' @examples
#' TODO
.behaviourAnalysisUI <- function(id) {
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
            id = ns("analysisResults"),
            solidHeader = TRUE,
            collapsible = FALSE, 
            title = "Analysis results",
            status = "primary",
            width = 12,
            uiOutput(ns("resultParams")),
            tabsetPanel(
              id = ns("analysisResultsTabs"),
              type = "tabs",
              # selected = "tracksClusters",
              selected = "hmm",
              tabPanel("HMM",
                       value = "hmm",
                       tags$br(),
                       uiOutput(ns("hmmPlots"))
              ),
              tabPanel("Tracks clusters",
                       value = "tracksClusters",
                       tags$br(),
                       uiOutput(ns("tracksClustersPlots"))
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
            title = "Gating parameters",
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
          populationUI(id, enableFilterPopulation = TRUE)
        )
      )
    )
  )
}
