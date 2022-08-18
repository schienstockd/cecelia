#' @description UI to segment images
#' @param id character of module ID
#' @examples
#' TODO
.segmentImagesUI <- function(id) {
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
        )
      ),
      column(
        3,
        fluidRow(
          .taskManagerUI(id)
        ),
        tags$br(),
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = TRUE, 
            title = "Segmentation parameters",
            status = "primary",
            width = 12,
            fluidRow(
              column(
                12,
                uiOutput(ns("taskFunction"))
              )
            ),
            uiOutput(ns("funParams")),
            # fluidRow(
            #   column(
            #     2, actionButton(ns("segmentVp"), "Segment viewpoint")
            #   )
            # )
          )
        ),
        # fluidRow(
        #   box(
        #     solidHeader = TRUE,
        #     collapsible = TRUE, 
        #     title = "Viewpoint",
        #     status = "primary",
        #     width = 12,
        #     collapsed = TRUE,
        #     fluidRow(
        #       column(
        #         12,
        #         tags$label("Padding"),
        #         fluidRow(
        #           column(6, sliderInput(ns("vpXYpadding"), "XY", 0, 500, 100, 50)),
        #           column(6, sliderInput(ns("vpZpadding"), "Z", 0, 50, 10, 5))
        #         )
        #       )
        #     ),
        #     fluidRow(
        #       column(
        #         12,
        #         tags$label("Snap"),
        #         fluidRow(
        #           column(6, sliderInput(ns("vpXYsnap"), "XY", 0, 500, 50, 10)),
        #           column(6, sliderInput(ns("vpZsnap"), "Z", 0, 50, 5, 5))
        #         )
        #       )
        #     )
        #   )
        # ),
      )
    )
  )
}
