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
      
      # add special type files
      extraFiles <- c()
      if (self$funParams()$specialType == "tenxXenium")
        extraFiles <- c("transcripts.csv.gz")
      
      # upload local files
      taskVars$fun <- list(
        localFiles = cciaObj$oriFilepath(),
        newFilename = fileIMAGE_TO_IMPORT,
        localDir = dirname(cciaObj$oriFilepath()),
        remoteDir = remoteDir,
        useCompression = FALSE,
        isSequence = self$funParams()$isSequence,
        extraFiles = extraFiles
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
