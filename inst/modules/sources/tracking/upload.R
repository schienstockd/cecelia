source(file.path(
  cfg$tasks$sources, "tracking.R")
)

Upload <- R6::R6Class(
  "Upload",
  inherit = Tracking,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "upload",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # init task launcher
      taskLauncher <- TaskLauncher$new()
      
      # modify ttask vars
      taskVars <- private$getTaskConf()
      
      # set remote environment
      localDir <- self$envParams("local")$dirs$task
      remoteDir <- self$envParams("hpc")$dirs$task
      
      localFiles <- c(
        taskDirFiles("labels", self$funParams()$valueNames),
        taskDirFiles("labelProps", self$funParams()$valueNames)
      )
      
      # upload local files
      taskVars$fun <- list(
        localFiles = file.path(
          localDir, localFiles),
        localDir = localDir,
        remoteDir = remoteDir,
        useCompression = FALSE
      )
      
      # run environment
      taskVars$env$global$env <- "local"
      runInplace <- TRUE
      
      self$initLog()
      self$writeLog(paste("Upload", localFiles))
      
      taskLauncher$initTask(
        "hpc.upload", taskVars, inplace = runInplace)
      
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