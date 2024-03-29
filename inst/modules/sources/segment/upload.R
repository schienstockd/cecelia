Upload <- R6::R6Class(
  "Upload",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "upload",
        sep = cecelia:::CCID_CLASS_SEP
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
      
      # get files
      imFiles <- basename(cciaObj$imFilepath())
      
      # upload local files
      taskVars$fun <- list(
        localFiles = file.path(localDir, imFiles),
        localDir = localDir,
        remoteDir = remoteDir,
        useCompression = FALSE
      )
      
      # run environment
      taskVars$env$global$env <- "local"
      runInplace <- TRUE
      
      self$initLog()
      # self$writeLog(paste("Upload", c(".ome.tiff", ".ome.zarr", ".zarr")))
      self$writeLog(paste("Upload", imFiles))
      
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
