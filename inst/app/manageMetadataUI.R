#' @description UI to manage metadata
#' @param id character of module ID
#' @examples
#' TODO
.manageMetadataUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  tagList(
    # useKeys(),
    # keysInput(ns("selectionHotkeys"), selectionHotkeys),
    
    fluidRow(
      column(
        9,
        box(
          solidHeader = TRUE,
          collapsible = FALSE, 
          title = "Manage metadata",
          status = "primary",
          width = 12,
          fluidRow(
            column(
              3,
              .imageSetUI(id)
            ),
            column(
              9,
              fluidRow(
                column(3, tags$label("Attribute Name")),
                column(3, tags$br()),
                column(3, tags$br()),
              ),
              fluidRow(
                column(3, textInput(ns("attributeName"), NULL)),
                column(3, actionButton(ns("createAttribute"), "Create Attribute")),
                column(3, actionButton(ns("resetMetadata"), "Load Metadata")),
              )
            )
          ),
          fluidRow(
            column(2, .imageSetAttributesUI(id)),
            column(
              10,
              dataTableOutput(ns("imageTable"))
            )
          )
        )
      ),
      column(
        3,
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = FALSE, 
            title = "Attributes",
            status = "primary",
            width = 12,
            tags$label("Select Attribute"),
            fluidRow(
              column(
                6,
                uiOutput(ns("selectAttribute"))
              ),
              column(
                6,
                shinyjs::disabled(actionButton(
                  ns("deleteAttribute"), "Remove Attribute",
                  class = btnCLASS_IMPORTANT))
              )
            ),
            fluidRow(
              column(
                12,
                tags$label("Specify value"),
                fluidRow(
                  column(
                    6,
                    shinyjs::disabled(textInput(ns("singleValue"), NULL))
                  ),
                  column(
                    6,
                    shinyjs::disabled(actionButton(ns("assignSingleValue"), "Assign Value"))
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                tags$label("Extract value by regular expression"),
                fluidRow(
                  column(
                    6,
                    shinyjs::disabled(textInput(ns("regexpValue"), NULL)),
                    radioButtons(ns("regexpSource"), "Extract from",
                                 choices = list(
                                   "Name of file" = "name",
                                   "Original filepath" = "oriFilepath"
                                 ), selected = "name")
                  ),
                  column(
                    6,
                    shinyjs::disabled(actionButton(ns("assignRegexpValue"), "Assign Regexp"))
                  )
                )
              )
            ),
            fluidRow(
              column(
                8,
                textAreaInput(ns("channelNameList"), "List channel names")
              ),
              column(
                4,
                tags$br(),
                actionButton(ns("assignChannelNameList"), "Assign channels")
              )
            ),
            fluidRow(
              column(
                12,
                actionButton(
                  ns("assignGroupSequences"), "Assign Group Sequences")
              )
            ),
            tags$br(),
            fluidRow(
              column(
                12,
                actionButton(
                  ns("copyChannelNamesToAll"), "Copy Channel names to all")
              )
            )
          )
        )
      )
    )
  )
}
