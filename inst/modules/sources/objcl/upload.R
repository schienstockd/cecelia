Upload <- R6::R6Class(
  "Upload",
  inherit = Objcl,
  
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
      localDir <- dirname(self$envParams("local")$dirs$task)
      remoteDir <- dirname(self$envParams("hpc")$dirs$task)
      
      # TODO classifiers are currently in the global image collection
      localDir <- file.path(localDir, CCID_IMAGE_COLLECTION)
      remoteDir <- paste(remoteDir, CCID_IMAGE_COLLECTION, sep = "/")
      
      # get local classification files
      localFiles <- list.files(file.path(
        localDir,
        cciaConf()$dirs$tasks$classifications,
        cciaConf()$dirs$classifications$pix
      ), full.names = TRUE)
      
      # upload classification files
      taskVars$fun <- list(
        localFiles = localFiles,
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
