#' @description Server to create new project
#' @import shinyFiles
#' 
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.projectSettingsServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Reactive values
      hpcOutput <- reactiveVal()
      hpcTask <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      
      ### Observers - RxAction
      ## Event specific
      # save inputs to settings
      observeEvent(input$projectName, {
        req(input$projectName)
        
        # save in manager
        globalManagers$projectManager()$setProjectName(
          input$projectName, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCaddress, {
        req(input$projectHPCaddress)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCaddress(
          input$projectHPCaddress, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCusername, {
        req(input$projectHPCusername)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCusername(
          input$projectHPCusername, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCsshKeyfile, {
        req(input$projectHPCsshKeyfile)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCsshKeyfile(
          input$projectHPCsshKeyfile, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCpartitionsCPU, {
        req(input$projectHPCpartitionsCPU)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCpartitionsCPU(
          input$projectHPCpartitionsCPU, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCpartitionsGPU, {
        req(input$projectHPCpartitionsGPU)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCpartitionsGPU(
          input$projectHPCpartitionsGPU, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCprojectCPU, {
        req(input$projectHPCprojectCPU)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCprojectCPU(
          input$projectHPCprojectCPU, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCqosGPU, {
        req(input$projectHPCqosGPU)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCqosGPU(
          input$projectHPCqosGPU, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCprojectCPU, {
        req(input$projectHPCprojectCPU)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCprojectCPU(
          input$projectHPCprojectCPU, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCprojectGPU, {
        req(input$projectHPCprojectGPU)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCprojectGPU(
          input$projectHPCprojectGPU, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCemail, {
        req(input$projectHPCemail)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCemail(
          input$projectHPCemail, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCemailOnBegin, {
        # save in manager
        globalManagers$projectManager()$setProjectHPCemailOnBegin(
          input$projectHPCemailOnBegin, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCemailOnEnd, {
        # save in manager
        globalManagers$projectManager()$setProjectHPCemailOnEnd(
          input$projectHPCemailOnEnd, invalidate = FALSE
        )
      })
      
      observeEvent(input$projectHPCemailOnFail, {
        # save in manager
        globalManagers$projectManager()$setProjectHPCemailOnFail(
          input$projectHPCemailOnFail, invalidate = FALSE
        )
      })
      
      # set input for keyfile
      observeEvent(input$projectHPCsshKeyfileChoose, {
        req(input$projectHPCsshKeyfileChoose)
        # check that a file was selected
        req("files" %in% names(input$projectHPCsshKeyfileChoose))
        
        curPath <- joinSelectedFile(input$projectHPCsshKeyfileChoose)
        
        # save in manager
        globalManagers$projectManager()$setProjectHPCsshKeyfile(
          curPath, invalidate = TRUE
        )
      })
      
      # update input value
      observeEvent(globalManagers$projectManager()$getProjectHPCsshKeyfile(), {
        req(globalManagers$projectManager()$getProjectHPCsshKeyfile())
        
        updateTextInput(
          session, "projectHPCsshKeyfile",
          value = globalManagers$projectManager()$getProjectHPCsshKeyfile()
        )
      })
      
      observeEvent(input$labServerSmbRemoteDir, {
        # save in manager
        globalManagers$projectManager()$setProjectLabServerSmbRemoteDir(
          input$labServerSmbRemoteDir, invalidate = FALSE
        )
      })
      
      observeEvent(input$labServerSmbLocalMountDir, {
        # save in manager
        globalManagers$projectManager()$setProjectLabServerSmbLocalMountDir(
          input$labServerSmbLocalMountDir, invalidate = FALSE
        )
      })
      
      observeEvent(input$labServerSmbUser, {
        # save in manager
        globalManagers$projectManager()$setProjectLabServerSmbUser(
          input$labServerSmbUser, invalidate = FALSE
        )
      })
      
      observeEvent(input$labServerSmbPwd, {
        # save in manager
        globalManagers$projectManager()$setProjectLabServerSmbPwd(
         input$labServerSmbPwd, invalidate = FALSE
        )
      })
      
      # hpc test result
      observeEvent(input$setupHPCTest, {
        # check if connection works
        conOK <- globalManagers$projectManager()$useHPC(reset = TRUE)
        
        toggleButtonBoolean(
          "setupHPCResult", conOK,
          btnCLASS_SUCCESS, btnCLASS_IMPORTANT,
          btnLabels = list(
            success = btnLABEL_SUCCESS,
            fail = btnLABEL_FAILED
          ))
        
        # enable update button
        if (conOK == TRUE) {
          enable("hpcTask")
          enable("runHPCTask")
        }
      })
      
      # update HPC user libraries
      observeEvent(input$runHPCTask, {
        req(input$hpcTask)
        
        taskLauncher <- TaskLauncher$new()
        
        hpcTask <- NULL
        
        # call hpc function
        if (input$hpcTask == "updateUserLibraries")
          hpcTask <- "hpc.updateUserLibraries"
        else if (input$hpcTask == "cleanupTransferredImages")
          hpcTask <- "hpc.cleanupTransferredImages"
        
        if (!is.null(hpcTask)) {
          # call update task on image analysis object set
          taskLauncher$initTask(
            hpcTask,
            createTaskVars(CCID_IMAGE_COLLECTION, globalManagers$projectManager(), "local"))
          
          # prep run
          taskLauncher$prepRun()
          
          # clear task log
          taskLauncher$clearTaskLogFile()
          
          # create task
          hpcTask(createForkedTask(
            list("Settings task" = taskLauncher)
          ))
          
          # clear output
          html(id = "hpcOutput", html = "*** START ***")
          disable("runHPCTask")
          enable("cancelHPCTask")
          
          # wait for exit
          o <- observe({
            outputFile <- hpcTask()$taskHandle()$taskLogFile()
            req(outputFile)
            
            # get log
            hpcOutput(readLogFile(
              outputFile, previousContent = hpcOutput()
            ))
            
            # add output
            if (!is.null(hpcOutput()) && attr(hpcOutput(), "updated") == TRUE) {
              # crop to a limit
              if (attr(hpcOutput(), "lineReads") > 100) {
                html(id = "hpcOutput",
                     html = trimws(paste(tail(hpcOutput(), 100), collapse = "\n")))
              } else {
                updatedContent <- trimws(paste(attr(hpcOutput(), "updatedContent"), collapse = "\n"))
                
                # only print if not only spaces
                # https://stackoverflow.com/a/35726243
                if (!grepl("^\\s*$", updatedContent))
                    html(id = "hpcOutput",
                         html = paste0("<br/>", updatedContent),
                         add = TRUE)
              }
              # }
            }
            
            # is task finished?
            if (hpcTask()$completed()) {
              o$destroy()
              
              # reset log
              hpcOutput(NULL)
              html(id = "hpcOutput", html = paste0("<br/>", "*** END ***"), add = TRUE)
              
              # reset buttons
              enable("runHPCTask")
              disable("cancelHPCTask")
            } else {
              invalidateLater(cciaConf()$tasks$log$poll)
            }
          })
        }
      })
      
      # cancel HPC user libraries
      observeEvent(input$cancelHPCTask, {
        req(hpcTask())
        
        # cancel task
        hpcTask()$cancel()
      })
      
      # activate lab server if HPC connection is working
      observeEvent(globalManagers$projectManager()$useHPC(), {
        if (globalManagers$projectManager()$useHPC() == TRUE) {
          enable("setupLabServerTest")
        } else {
          disable("setupLabServerTest")
        }
      })
      
      # lab server test result
      observeEvent(input$setupLabServerTest, {
        # check if connection works
        conOK <- globalManagers$projectManager()$testLabServerConnection()
        
        toggleButtonBoolean(
          "setupLabServerResult", conOK,
          btnCLASS_SUCCESS, btnCLASS_IMPORTANT,
          btnLabels = list(
            success = btnLABEL_SUCCESS,
            fail = btnLABEL_FAILED
          ))
      })
      
      ## Generic
      
      ### UI Outputs
      ## Tables
      
      ## Plots
      
      ## Buttons
      
      ## Other
      # project ID
      output$projectUID <- renderUI({
        req(globalManagers$projectManager()$getProjectUID())
        
        disabled(textInput(
          session$ns("projectUID"), "unique ID (uID):",
          globalManagers$projectManager()$getProjectUID()
        ))
      })
      
      # project Name
      output$projectName <- renderUI({
        req(globalManagers$projectManager()$getProjectName())
        
        textInput(
          session$ns("projectName"),
          "Name", globalManagers$projectManager()$getProjectName()
        )
      })
      
      # keyfile
      shinyFileChoose(
        input, "projectHPCsshKeyfileChoose",
        roots = shinyFiles::getVolumes(),
        session = session,
        hidden = TRUE)
    }
  )
}
