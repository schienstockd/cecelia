#' @description UI to create new project
#' @import shinyFiles
#' 
#' @param id character of module ID
#' @examples
#' TODO
.projectSettingsUI <- function(id) {
  # every file starts with the namespace
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Project", status = "primary", solidHeader = TRUE,
        fluidRow(
          column(
            7,
            uiOutput(ns("projectName"))
          ),
          column(
            5,
            uiOutput(ns("projectUID"))
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "HPC", status = "primary", solidHeader = TRUE,
        fluidRow(
          column(
            7,
            textInput(ns("projectHPCaddress"), "Address",
                      value = cciaConf()$hpc$address)
          ),
          column(
            5,
            textInput(ns("projectHPCusername"), "Username")
          )
        ),
        tags$label("SSH Keyfile"),
        fluidRow(
          column(
            5,
            textInput(ns("projectHPCsshKeyfile"), NULL, value = "")
          ),
          column(
            2,
            shinyFilesButton(
              ns("projectHPCsshKeyfileChoose"),
              "Select", NULL, multiple = FALSE)
          )
        ),
        fluidRow(
          column(
            6,
            textInput(ns("projectHPCpartitionsCPU"), "CPU partitions",
                      value = cciaConf()$hpc$partitions$cpu),
            textInput(ns("projectHPCpartitionsGPU"), "GPU partitions",
                      value = cciaConf()$hpc$partitions$gpu)
          ),
          column(
            3,
            textInput(ns("projectHPCqosCPU"), "CPU qos",
                      value = cciaConf()$hpc$qos$cpu),
            textInput(ns("projectHPCqosGPU"), "GPU qos",
                      value = cciaConf()$hpc$qos$gpu)
          ),
          column(
            3,
            textInput(ns("projectHPCprojectCPU"), "CPU project",
                      value = cciaConf()$hpc$project$cpu),
            textInput(ns("projectHPCprojectGPU"), "GPU project",
                      value = cciaConf()$hpc$project$gpu)
          )
        ),
        tags$label("Email notifications"),
        fluidRow(
          column(
            7,
            textInput(ns("projectHPCemail"), NULL,
                      value = "")
          ),
          column(
            5,
            fillRow(
              checkboxInput(
                ns("projectHPCemailOnBegin"), "Begin", value = FALSE),
              checkboxInput(
                ns("projectHPCemailOnEnd"), "End", value = FALSE),
              checkboxInput(
                ns("projectHPCemailOnFail"), "Fail", value = TRUE)
            )
          )
        ),
        fluidRow(
          column(
            2,
            actionButton(ns("setupHPCTest"), "Test connection")
          ),
          column(
            1,
            actionButton(ns("setupHPCResult"), "...")
          ),
          column(
            3,
            disabled(selectInput(
              ns("hpcTask"), NULL,
              choices = list(
                "Update user libraries" = "updateUserLibraries",
                "Update Cecelia package" = "updateCecelia",
                "Cleanup transferred images" = "cleanupTransferredImages"
              ), selected = "updateUserLibraries"
              ))
          ),
          column(
            1,
            disabled(actionButton(ns("runHPCTask"), "Run"))
          ),
          column(
            1,
            disabled(actionButton(ns("cancelHPCTask"), "Cancel",
                         class = btnCLASS_IMPORTANT))
          )
        ),
        tags$hr(),
        fluidRow(
          column(12,
                 # https://stackoverflow.com/a/48500417
                 div(style = "max-height:300px;overflow-y:scroll",
                     verbatimTextOutput(ns("hpcOutput")))
          )
        )
      ),
      box(
        title = "Lab Server", status = "primary", solidHeader = TRUE,
        fluidRow(
          column(
            4,
            textInput(
              ns("labServerSmbRemoteDir"), "Server address")
          ),
          column(
            2,
            textInput(
              ns("labServerSmbRemoteAddon"), "Lab directory")
          ),
          column(
            4,
            textInput(
              ns("labServerSmbLocalMountDir"), "Local mounting directory")
          )
        ),
        fluidRow(
          column(
            6,
            textInput(
              ns("labServerSmbUser"), "[DOMAIN/]Username")
          ),
          column(
            4,
            passwordInput(ns("labServerSmbPwd"), "Password")
          )
        ),
        fluidRow(
          column(
            3,
            disabled(actionButton(ns("setupLabServerTest"), "Test connection"))
          ),
          column(
            3,
            actionButton(ns("setupLabServerResult"), "...")
          )
        )
      ),
      box(
        title = "Mediaflux", status = "primary", solidHeader = TRUE,
        fluidRow(
          column(
            8,
            textInput(
              ns("mfluxHost"), "Host", value = "")
          ),
          column(
            2,
            textInput(
              ns("mfluxPort"), "Port", value = "443")
          ),
          column(
            2,
            textInput(
              ns("mfluxTransport"), "Transport", value = "https")
          )
        ),
        fluidRow(
          column(
            12,
            textInput(
              ns("mfluxNamespace"), "Namespace", value = "")
          )
        ),
        tags$label("Token Keyfile"),
        fluidRow(
          column(
            5,
            textInput(ns("mfluxTokenFile"), NULL, value = "")
          ),
          column(
            2,
            shinyFilesButton(
              ns("mfluxTokenFileChoose"),
              "Select", NULL, multiple = FALSE)
          )
        ),
        fluidRow(
          column(
            4,
            textInput(
              ns("mfluxUsername"), "Username", value = "")
          ),
          column(
            3,
            sliderInput(
              ns("mfluxNbWorkers"), "Number of Workers", min = 1, max = 8, value = 4)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'retrieveProject'", ns("mfluxTask")),
            column(2, textInput(ns("mfluxRetrPID"), "pID to retrieve", value = ""))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'uploadProject'", ns("mfluxTask")),
            column(2, checkboxInput(ns("mfluxSync"), "Sync", value = FALSE))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'transferProject'", ns("mfluxTask")),
            column(2, textInput(ns("mfluxTransferDir"), "Transfer directory", value = ""))
          )
        ),
        fluidRow(
          # column(
          #   2,
          #   actionButton(ns("setupHPCTest"), "Test connection")
          # ),
          # column(
          #   1,
          #   actionButton(ns("setupMfluxResult"), "...")
          # ),
          column(
            3,
            selectInput(
              ns("mfluxTask"), NULL,
              choices = list(
                "Upload project" = "uploadProject",
                "Retrieve project" = "retrieveProject",
                "Transfer project" = "transferProject"
              ), selected = "backupProject"
            )
          ),
          column(
            1,
            actionButton(ns("runMfluxTask"), "Run")
          ),
          column(
            1,
            actionButton(ns("cancelMfluxTask"), "Cancel", class = btnCLASS_IMPORTANT)
          )
        ),
        tags$hr(),
        fluidRow(
          column(12,
                 div(style = "max-height:300px;overflow-y:scroll",
                     verbatimTextOutput(ns("mfluxOutput")))
          )
        )
      )
    )
  )
}
