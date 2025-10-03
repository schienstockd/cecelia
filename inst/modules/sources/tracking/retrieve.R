Retrieve <- R6::R6Class(
  "Retrieve",
  inherit = Tracking,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "retrieve",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # init task launcher
      taskLauncher <- TaskLauncher$new()
      
      # modify task vars
      taskVars <- private$getTaskConf()
      
      # set remote environment
      localDir <- self$envParams("local")$dirs$task
      remoteDir <- self$envParams("hpc")$dirs$task
      
      # define labels file
      filesToGet <- c(
        taskDirFiles("labelProps", self$funParams()$valueNames)
      )
      
      # retrieve remote files
      taskVars$fun <- list(
        remoteFiles = filesToGet,
        localDir = localDir,
        remoteDir = remoteDir
      )
      
      # run environment
      taskVars$env$global$env <- "local"
      runInplace <- TRUE
      
      self$initLog()
      self$writeLog("Retrieve")
      self$writeLog(paste(filesToGet, collapse = "\n"))
      
      taskLauncher$initTask(
        "hpc.retrieve", taskVars, inplace = runInplace)
      
      # prep run
      taskLauncher$prepRun()
      
      # run task
      taskLauncher$run()
      
      # update image info
      self$updateImageInfo()
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
