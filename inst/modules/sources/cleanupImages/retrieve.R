source(file.path(
  cfg$tasks$sources, "cleanupImages.R")
)

Retrieve <- R6::R6Class(
  "Retrieve",
  inherit = CleanupImages,
  
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
      localDir <- self$envParams("local")$dirs$zero
      remoteDir <- self$envParams("hpc")$dirs$zero
      # if (self$funParams()$remoteEnv == "hpc")
      
      # upload local files
      taskVars$fun <- list(
        remoteFiles = c(
          "ccidCorrected.zarr"
        ),
        localDir = localDir,
        remoteDir = remoteDir,
        useCompression = TRUE,
        useArchive = TRUE
      )
      
      # run environment
      taskVars$env$global$env <- "local"
      runInplace <- TRUE
      
      self$initLog()
      self$writeLog(paste("Retrieve"))
      
      taskLauncher$initTask(
        "hpc.retrieve", taskVars, inplace = runInplace)
      
      # prep run
      taskLauncher$prepRun()
      
      # run task
      taskLauncher$run()

      self$writeLog(taskLauncher$result(TRUE))
      
      # update image information
      self$updateImageInfo("AF generated")
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)