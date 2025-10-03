Upload <- R6::R6Class(
  "Upload",
  inherit = SpatialAnalysis,
  
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
      
      self$initLog()
      
      # init task launcher
      taskLauncher <- TaskLauncher$new()
      
      # modify ttask vars
      taskVars <- private$getTaskConf()
      
      # set remote environment
      localDir <- self$envParams("local")$dirs$task
      remoteDir <- self$envParams("hpc")$dirs$task
      
      localFiles <- c(
        file.path(cciaConf()$dirs$tasks$labelProps, sapply(
          self$funParams()$valueNames,
          function(x) basename(cciaObj$imLabelPropsFilepath(x))
          )),
        file.path(cciaConf()$dirs$tasks$labelProps, sapply(
          self$funParams()$adataNames,
          function(x) basename(cciaObj$imAnndataFilepath(x))
          ))
      )
      
      # add histocytometry data
      if (self$funParams()$popType == "flow") {
        gsFiles <- sapply(
          self$funParams()$valueNames,
          function(x) basename(cciaObj$imGatingSetFilepath(x))
        )
        
        localFiles <- c(localFiles, file.path(cciaConf()$dirs$tasks$data, gsFiles))
        
        # remove gs before upload
        taskVars$fun <- list(
          dirs = paste0(remoteDir, "/", cciaConf()$dirs$tasks$data, "/", gsFiles)
        )
        
        # run environment
        taskVars$env$global$env <- "local"
        
        self$writeLog(paste(
          "Remove", paste(paste0(remoteDir, "/", cciaConf()$dirs$tasks$data, "/", gsFiles), collapse = ":")))
        
        # run task
        taskLauncher$initTask("hpc.rmDirs", taskVars, inplace = TRUE)
        taskLauncher$prepRun()
        taskLauncher$run()
        taskLauncher$result(TRUE)
      }
      
      # upload local files
      taskVars$fun <- list(
        localFiles = file.path(
          localDir, localFiles),
        localDir = localDir,
        remoteDir = remoteDir,
        useCompression = FALSE
      )
      
      self$writeLog(paste("Upload", localFiles))
      
      # run environment
      taskVars$env$global$env <- "local"
      
      # run task
      taskLauncher$initTask("hpc.upload", taskVars, inplace = TRUE)
      taskLauncher$prepRun()
      taskLauncher$run()
      taskLauncher$result(TRUE)
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
