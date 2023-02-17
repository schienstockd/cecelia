Upload <- R6::R6Class(
  "Upload",
  inherit = Import,
  
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
      remoteDir <- self$envParams("hpc")$dirs$zero
      # if (self$funParams()$remoteEnv == "hpc")
      
      filesToCopy <- cciaObj$oriFilepath()
      
      newFileNames <- list.files(dirname(oldFilename),
                                 pattern = sprintf(".%s$", fileExt))
        fileIMAGE_TO_IMPORT,
        isSequence = self$funParams()$isSequence
      )
      
      # upload local files
      taskVars$fun <- list(
        localFiles = filesToCopy$files,
        localDir = dirname(cciaObj$oriFilepath()),
        remoteDir = remoteDir,
        useCompression = FALSE
      )
      
      # run environment
      taskVars$env$global$env <- "local"
      runInplace <- TRUE
      
      self$initLog()
      self$writeLog(paste("Upload", cciaObj$oriFilepath()))
      
      self$writeLog(self$funParams()$remoteEnv)
      
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
