Retrieve <- R6::R6Class(
  "Retrieve",
  inherit = Import,
  
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
      self$resetImageInfo()
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # init task launcher
      taskLauncher <- TaskLauncher$new()
      
      # modify task vars
      taskVars <- private$getTaskConf()
      
      # set remote environment
      localDir <- self$envParams("local")$dirs$zero
      remoteDir <- self$envParams("hpc")$dirs$zero
      # if (self$funParams()$remoteEnv == "hpc")
      
      # define file extension
      syncExt <- ".ome.zarr"
      
      if (self$funParams()$syncFile == "ometiff")
        syncExt <- ".ome.tiff"
      else if (self$funParams()$syncFile == "cell2location")
        syncExt <- ".zarr"
      
      # retrieve
      taskVars$fun <- list(
        remoteFiles = fileIMAGE_CONVERTED,
        localDir = localDir,
        remoteDir = remoteDir,
        # syncExt = c(".ome.tiff", ".ome.zarr"),
        syncExt = syncExt,
        useCompression = TRUE,
        useArchive = TRUE
      )
      
      # run environment
      taskVars$env$global$env <- "local"
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
      
      # get label properties
      if (self$funParams()$syncFile == "cell2location") {
        # TODO this is hard coded
        funParams <- list(
          remoteFiles = taskDirFiles("labelProps", "default"),
          localDir = self$envParams("local")$dirs$task,
          remoteDir = self$envParams("hpc")$dirs$task
        )
        
        self$runTasks(c("hpc.retrieve"), funParams = funParams)
      }
      
      # update image information
      self$updateImageInfo()
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
