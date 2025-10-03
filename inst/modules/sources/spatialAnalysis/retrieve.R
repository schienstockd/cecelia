Retrieve <- R6::R6Class(
  "Retrieve",
  inherit = SpatialAnalysis,
  
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
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # init task launcher
      taskLauncher <- TaskLauncher$new()
      
      # modify task vars
      taskVars <- private$getTaskConf()
      
      # set remote environment
      localDir <- self$envParams("local")$dirs$task
      remoteDir <- self$envParams("hpc")$dirs$task
      
      if (self$funParams()$callingFun %in% c(
        "cellClusters", "cellClustersMeshes", "cellContactsMeshes")
        ) {
        # define regions and labels file
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
        
        self$writeLog(taskLauncher$result(TRUE))
        
        # retrieve ccia object to get generated populations
        self$runTasks("hpc.retrieveCciaObj")
      } else if (self$funParams()$callingFun == "cellRegions") {
        # define regions and labels file
        filesToGet <- c(
          taskDirFiles(
            "labelProps", paste("regions", self$funParams()$regionValueNames, sep = "_")
          )
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
        
        self$writeLog(taskLauncher$result(TRUE))
        
        # update image information
        for (x in self$funParams()$regionValueNames) {
          cciaObj$setImRegionsFilepath(
            basename(taskDirFiles(
              "labelProps", paste("regions", x, sep = "_")
            )),
            valueName = x
          )
        }
        
        cciaObj$saveState()
      }
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
