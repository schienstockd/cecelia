#' @descriptionUI UI to manage tasks
#' @param id character of module ID
#' @examples
#' TODO
.taskManagerUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  tagList(
    box(
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Task Manager",
      status = "primary",
      width = 12,
      fluidRow(
        # column(
        #   6,
        #   tags$label("All"),
        #   fluidRow(
        #     column(
        #       4,
        #       actionButton(ns("taskRunAll"), "Run")
        #     ),
        #     column(
        #       8,
        #       disabled(actionButton(
        #         ns("taskCancelAll"), "Cancel",
        #         class = btnCLASS_IMPORTANT))
        #     )
        #   )
        # ),
        column(
          6,
          tags$label("Selected"),
          fluidRow(
            column(
              4,
              disabled(actionButton(
                ns("taskRunSelected"), "Run"))
            ),
            column(
              8,
              disabled(actionButton(
                ns("taskCancelShown"), "Cancel shown",
                class = btnCLASS_IMPORTANT))
            )
            # column(
            #   8,
            #   disabled(actionButton(
            #     ns("taskCancelSelected"), "Cancel",
            #     class = btnCLASS_IMPORTANT))
            # )
          ),
          tags$hr(),
          fluidRow(
            column(
              4,
              actionButton(ns("taskClearTasks"), "Clear Tasks")
            )
          )
        ),
        column(
          6,
          fluidRow(
            column(
              12,
              uiOutput(ns("taskLimit")),
              checkboxInput(
                ns("taskUpdateImage"),
                "Update Image",
                value = FALSE),
              disabled(checkboxInput(
                ns("runTaskCombined"),
                "Combine images",
                value = FALSE)),
              checkboxInput(
                ns("runTaskOnSet"),
                "Run together",
                value = FALSE)
            )
          )
        )
      ),
      tags$label("Environment"),
      tabsetPanel(
        id = ns("taskEnvironment"),
        type = "tabs",
        selected = "hpc",
        tabPanel(
          "local", value = "local",
          tags$br(),
          fluidRow(
            column(
              6,
              checkboxInput(ns("taskLocalUseGPU"), "Use GPU", value = FALSE)
            )
          )
        ),
        tabPanel(
          "hpc", value = "hpc",
          tags$br(),
          fluidRow(
            column(
              6,
              checkboxInput(ns("taskHPCuseGPU"), "Use GPU", value = FALSE),
              checkboxInput(ns("uploadCciaObj"), "Upload local object", value = TRUE)
            ),
            column(
              6,
              actionButton(ns("taskHPCsync"), "Sync HPC tasks")
            )
          ),
          fluidRow(
            column(
              6,
              textInput(ns("taskHPCnumNodes"), "Nodes", value = "1"),
              textInput(ns("taskHPCnumCPUperTask"), "CPUs", value = "1"),
              textInput(ns("taskHPCmemory"), HTML("Memory<br/>(GB)"), value = "20")
            ),
            column(
              6,
              textInput(ns("taskHPCnumTasks"), "Tasks", value = "1"),
              textInput(ns("taskHPCnumGPUperTask"), "GPUs", value = "1"),
              textInput(ns("taskHPCwalltime"), HTML("Walltime<br/>(dd-hh:mm:ss)"), value = "00-01:00:00")
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          checkboxInput(ns("taskMonitorShowLastTasks"), "Last tasks", value = TRUE),
          uiOutput(ns("taskMonitorUIDs"))
        ),
        column(
          6,
          checkboxInput(ns("taskMonitorShowSelectedFunction"), "Selected function", value = TRUE),
          uiOutput(ns("taskMonitorTaskFunctions"))
        )
      ),
      # filters for task monitor
      DT::dataTableOutput(ns("taskMonitorAll"))
      # tabsetPanel(
      #   id = ns("taskMonitor"),
      #   type = "tabs",
      #   selected = "all",
      #   tabPanel("all", value = "all", DT::dataTableOutput(ns("taskMonitorAll"))),
      #   tabPanel("running", value = "running", DT::dataTableOutput(ns("taskMonitorRunning"))),
      #   tabPanel("queued", value = "queued", DT::dataTableOutput(ns("taskMonitorQueued"))),
      #   tabPanel("success", value = "success", DT::dataTableOutput(ns("taskMonitorSuccess"))),
      #   tabPanel("failed", value = "failed", DT::dataTableOutput(ns("taskMonitorFailed")))
      # )
    )
  )
}
