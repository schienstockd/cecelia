#' @description UI plot images
#' @param id character of module ID
#' @examples
#' TODO
.imageViewerUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  tagList(
    box(
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Image Viewer",
      status = "primary",
      width = 12,
      fluidRow(
        column(
          6,
          checkboxInput(
            ns("lazyLoading"), "Lazy Loading")
        )
      )
    )
  )
}