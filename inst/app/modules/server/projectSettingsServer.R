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
      ### Functions
      
      # create task observer
      createTaskObserver <- function(taskType, taskInput, taskReactive, outputID,
                                     outputReactive, funParams = NULL, finalExp = NULL) {
        taskLauncher <- TaskLauncher$new()
        
        # call function
        taskName <- paste0(taskType, ".", input[[taskInput]])
        
        if (!is.null(taskName)) {
          # get task vars
          taskVars <- createTaskVars(cecelia:::CCID_IMAGE_COLLECTION, globalManagers$projectManager(), "local")
          
          # add function parameters
          if (!is.null(funParams)) 
            taskVars$fun <- funParams
          
          # call update task on image analysis object set
          taskLauncher$initTask(taskName, taskVars)
          
          # prep run
          taskLauncher$prepRun()
          
          # clear task log
          taskLauncher$clearTaskLogFile()
          
          # create task
          taskReactive(createForkedTask(
            list("Settings task" = taskLauncher)
          ))
          
          # clear output
          html(id = outputID, html = "*** START ***")
          disable(paste0("run", firstToupper(taskInput)))
          enable(paste0("cancel", firstToupper(taskInput)))
          
          # wait for exit
          o <- observe({
            outputFile <- taskReactive()$taskHandle()$taskLogFile()
            req(outputFile)
            
            # get log
            outputReactive(readLogFile(
              outputFile, previousContent = outputReactive()
            ))
            
            # add output
            if (!is.null(outputReactive()) && attr(outputReactive(), "updated") == TRUE) {
              # crop to a limit
              if (attr(outputReactive(), "lineReads") > 100 || length(attr(outputReactive(), "updatedContent")) > 100) {
                html(id = "outputReactive",
                     html = trimws(paste(tail(outputReactive(), 100), collapse = "\n")))
              } else {
                updatedContent <- trimws(paste(attr(outputReactive(), "updatedContent"), collapse = "\n"))
                
                # only print if not only spaces
                # https://stackoverflow.com/a/35726243
                if (!grepl("^\\s*$", updatedContent))
                  html(id = outputID,
                       html = paste0("<br/>", updatedContent),
                       add = TRUE)
              }
            }
            
            # is task finished?
            if (taskReactive()$completed()) {
              o$destroy()
              
              # reset log
              outputReactive(NULL)
              html(id = outputID, html = paste0("<br/>", "*** END ***"), add = TRUE)
              
              # reset buttons
              enable(paste0("run", firstToupper(taskInput)))
              disable(paste0("cancel", firstToupper(taskInput)))
              
              # call final expression
              if (!is.null(finalExp)) {
                eval(parse(text = finalExp))
              }
            } else {
              invalidateLater(cciaConf()$tasks$log$poll)
            }
          })
        }
      }
      
      ### Reactive values
      hpcOutput <- reactiveVal()
      hpcTask <- reactiveVal()
      
      mfluxOutput <- reactiveVal()
      mfluxTask <- reactiveVal()
      
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
      
      observeEvent(input$labServerSmbRemoteAddon, {
        # save in manager
        globalManagers$projectManager()$setProjectLabServerSmbRemoteAddon(
          input$labServerSmbRemoteAddon, invalidate = FALSE
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
      
      observeEvent(input$mfluxHost, {
        # save in manager
        globalManagers$projectManager()$setProjectMfluxHost(
          input$mfluxHost, invalidate = FALSE
        )
      })
      
      observeEvent(input$mfluxPort, {
        # save in manager
        globalManagers$projectManager()$setProjectMfluxPort(
          input$mfluxPort, invalidate = FALSE
        )
      })
      
      observeEvent(input$mfluxTransport, {
        # save in manager
        globalManagers$projectManager()$setProjectMfluxTransport(
          input$mfluxTransport, invalidate = FALSE
        )
      })
      
      observeEvent(input$mfluxNamespace, {
        # save in manager
        globalManagers$projectManager()$setProjectMfluxNamespace(
          input$mfluxNamespace, invalidate = FALSE
        )
      })
      
      observeEvent(input$mfluxUsername, {
        # save in manager
        globalManagers$projectManager()$setProjectMfluxUsername(
          input$mfluxUsername, invalidate = FALSE
        )
      })
      
      # set input for token file
      observeEvent(input$mfluxTokenFileChoose, {
        req(input$mfluxTokenFileChoose)
        # check that a file was selected
        req("files" %in% names(input$mfluxTokenFileChoose))
        
        curPath <- joinSelectedFile(input$mfluxTokenFileChoose)
        
        # save in manager
        globalManagers$projectManager()$setProjectMfluxTokenFile(
          curPath, invalidate = TRUE
        )
      })
      
      # update input value
      observeEvent(globalManagers$projectManager()$getProjectMfluxTokenFile(), {
        req(globalManagers$projectManager()$getProjectMfluxTokenFile())
        
        updateTextInput(
          session, "mfluxTokenFile",
          value = globalManagers$projectManager()$getProjectMfluxTokenFile()
        )
      })
      
      observeEvent(input$mfluxNbWorkers, {
        # save in manager
        globalManagers$projectManager()$setProjectMfluxNbWorkers(
          input$mfluxNbWorkers, invalidate = FALSE
        )
      })
      
      observeEvent(input$mfluxSync, {
        # save in manager
        globalManagers$projectManager()$setProjectMfluxSync(
          input$mfluxSync, invalidate = FALSE
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
        
        createTaskObserver("hpc", "hpcTask", hpcTask, "hpcOutput", hpcOutput)
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
      
      # call Mediaflux tasks
      observeEvent(input$runMfluxTask, {
        req(input$mfluxTask)
        
        # check to prepare project upload
        if (input$mfluxTask == "uploadProject")
          globalManagers$projectManager()$exportProject(saveData = FALSE)
        
        funParams <- list()
        finalExp <- NULL
        
        # fun params
        if (input$mfluxTask %in% c("retrieveProject", "recoverProject")) {
          funParams <- append(funParams, list(retrPID = input$mfluxRetrPID))
        }
        
        if (input$mfluxTask %in% c("backupProject", "recoverProject")) {
          req(input$mfluxBackupDir)
          funParams <- append(funParams, list(pDir = input$mfluxBackupDir))
        }
        
        # expressions
        if (input$mfluxTask == "retrieveProject") {
          req(input$mfluxRetrPID)
          
          # create expression for completion
          finalExp <- sprintf(
            "globalManagers$projectManager()$importProject('%s', retrFiles = FALSE)",
            input$mfluxRetrPID)
        } 
        
        createTaskObserver("mflux", "mfluxTask", mfluxTask, "mfluxOutput",
                           mfluxOutput, funParams = funParams, finalExp = finalExp)
      })
      
      # cancel Mediaflux
      observeEvent(input$cancelMfluxTask, {
        req(mfluxTask())
        
        # cancel task
        mfluxTask()$cancel()
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
      
      # tokenfile
      shinyFileChoose(
        input, "mfluxTokenFileChoose",
        roots = shinyFiles::getVolumes(),
        session = session,
        hidden = TRUE)
    }
  )
}
