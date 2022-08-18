Retrieve <- R6::R6Class(
  "Retrieve",
  inherit = ClustPopulations,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "retrieve",
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
      # if (self$funParams()$remoteEnv == "hpc")
      
      # upload local files
      taskVars$fun <- list(
        remoteFiles = taskDirFiles(
          "labelProps",
          paste0(self$funParams()$valueName, ".clust")
          ),
        localDir = localDir,
        remoteDir = remoteDir
      )
      
      # run environment
      taskVars$env$global$env <- "local"
      runInplace <- TRUE
      
      self$initLog()
      self$writeLog(paste("Retrieve", "adata.h5ad"))
      
      taskLauncher$initTask(
        "hpc.retrieve", taskVars, inplace = runInplace)
      
      # prep run
      taskLauncher$prepRun()
      
      # run task
      taskLauncher$run()
      
      taskLauncher$result(TRUE)
      
      # update image information
      # self$updateImageInfo()
      
      # get updated object from remote
      self$runTasks("hpc.retrieveCciaObj")
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
