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
      
      self$writeLog(self$envParams("local")$dirs$task)
      self$writeLog(self$envParams("hpc")$dirs$task)
      
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
        retrPID = self$globalEnv()$pID,
        pDir = self$funParams()$pDir,
        mfluxConfigFile = paste0(self$envParams("hpc")$dirs$task, "/", mfluxConfigFile),
        smbConfigFile = paste0(self$envParams("hpc")$dirs$task, "/", smbConfigFile),
      )
      
      # run environment
      taskVars$env$global$env <- "hpc"
      runInplace <- TRUE
      
      self$initLog()
      self$writeLog(paste("Retrieve", fileIMAGE_CONVERTED))
      
      taskLauncher$initTask(
        "hpc.retrieve", taskVars, inplace = runInplace)
      
      # prep run
      taskLauncher$prepRun()
      
      # run task
      taskLauncher$run()
      
      taskLauncher$result(TRUE)
      
      # mount lab server
      
      # download data from mediaflux to dataserver
      
      # close connection
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
