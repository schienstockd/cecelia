TransferProject <- R6::R6Class(
  "TransferProject",
  inherit = Mflux,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "transferProject",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$initLog()
      self$writeLog("Transfer")
      
      # need to push config file to server to access mediaflux
      mfluxConfigFile <- self$prepMfluxConfig(
        tmpdir = file.path(self$envParams()$dirs$task, "tasks"))
      
      # push lab server credentials to HPC
      smbConfigFile <- self$prepSmbConfig(
        tmpdir = file.path(self$envParams()$dirs$task, "tasks"))
      configFiles <- c(mfluxConfigFile, smbConfigFile)
      
      # transfer configs
      funParams <- list(
        localFiles = configFiles,
        localDir = self$envParams("local")$dirs$task,
        remoteDir = self$envParams("hpc")$dirs$task,
        useCompression = FALSE
      )
      self$runTasks(c("hpc.upload"), funParams = funParams)
      
      # unlink configs
      unlink(configFiles)
      
      # start job for transfer
      taskVars <- private$getTaskConf()
      taskVars$fun <- list(
        retrPID = self$globalParams()$pID,
        pDir = self$funParams()$pDir,
        mfluxConfigFile = paste0(self$envParams("hpc")$dirs$task, "/tasks/", basename(mfluxConfigFile)),
        smbConfigFile = paste0(self$envParams("hpc")$dirs$task, "/tasks/", basename(smbConfigFile))
      )
      
      # run environment
      taskVars$env$global$env <- "hpc"
      
      # switch task dirs
      # taskVars$env$local$dirs$task <- taskVars$env$hpc$dirs$task
      # taskVars$env$local$dirs$zero <- taskVars$env$hpc$dirs$zero
      
      # init task launcher
      taskLauncher <- TaskLauncher$new()
      taskLauncher$initTask("mflux.retrieveProject", taskVars, inplace = FALSE)
      
      # prep run
      taskLauncher$prepRun()
      
      # run task
      taskLauncher$run()
      
      taskLauncher$result(TRUE)
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
