#' @description UI to create new project
#' @import shinyFiles
#' 
#' @param id character of module ID
#' @examples
#' TODO
.createProjectUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  # get random name
  randName <- sample(cciaConf()$random$names$animals, 1)
  
  tagList(
    fluidRow(
      column(4,
             textInput(ns("projectName"), "Select name",
                       paste0(randName, "'s imaging project")
                       ),
             )
    ),
    # fluidRow(
    #   column(4,
    #          shinyDirButton(
    #            ns("projectPath"), label = "Select directory",
    #            title = 'Select directory')
    #          )
    # ),
    tags$br(),
    fluidRow(
      column(4,
        radioButtons(
          ns("projectType"), "Select type:",
          c(
            "Static image" = "static",
            "Live cell image" = "live",
            "Flow Cytometry" = "flow"
            ))
      )
    ),
    fluidRow(
      column(2,
             actionButton(ns("createProject"), "Create project")
      )
    )
  )
}
