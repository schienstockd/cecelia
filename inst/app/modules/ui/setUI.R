#' @descriptionUI UI to manage image sets
#' @param id character of module ID
#' @param enableAddition boolean of to enable addition of sets
#' @param enableDeletion boolean of to enable deletion of sets
#' @examples
#' TODO
.imageSetUI <- function(
  id, enableAddition = FALSE, enableDeletion = FALSE) {
  # every file starts with the namespace
  ns <- NS(id)
  
  # prepare fields
  fieldLabels <- list()
  fieldInputs <- list()
  
  colWidthAdj <- 1
  
  if (enableAddition && enableDeletion) {
    colWidthAdj <- 3
  } else if (xor(enableAddition, enableDeletion)) {
    ycolWidthAdj <- 2
  }
  
  # add sets
  if (enableAddition == TRUE) {
    fieldLabels <- append(
      fieldLabels,
      list(
        column(12/colWidthAdj, tags$label("Set Name")),
        column(12/(colWidthAdj * 2), tags$br())
      )
    )
    fieldInputs <- append(
      fieldInputs,
      list(
        column(12/colWidthAdj, textInput(ns("setName"), NULL)),
        column(12/(colWidthAdj * 2), actionButton(ns("createSet"), "Create Set"))
      )
    )
  }
  
  # select sets
  fieldLabels <- append(
    fieldLabels,
    list(
      column(12/colWidthAdj,
             tags$label("Select Set"),
             # tags$label(" ", style = "width:1em"),
             tags$label(" | "),
             actionLink(ns("reloadSet"), NULL, icon = icon(btnICON_RELOAD)),
             actionLink(ns("resetSet"), NULL, icon = icon(btnICON_RESET)),
             actionLink(ns("forceDataReloadForSet"), NULL, icon = icon(btnICON_IMPORT)),
      )
    )
  )
  
  fieldInputs <- append(
    fieldInputs,
    list(
      column(12/colWidthAdj, disabled(uiOutput(ns("selectSet"))))
    )
  )
  
  # delete sets
  if (enableDeletion == TRUE) {
    fieldLabels <- append(
      fieldLabels,
      list(
        column(12/(colWidthAdj * 2), tags$br())
      )
    )
    fieldInputs <- append(
      fieldInputs,
      list(
        column(12/(colWidthAdj * 2), disabled(actionButton(
          ns("deleteSet"), "Delete Set", class = btnCLASS_IMPORTANT)))
      )
    )
  }
  
  tagList(
    fluidRow(fieldLabels),
    fluidRow(fieldInputs)
  )
}
