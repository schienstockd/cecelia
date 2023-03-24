Retrieve <- R6::R6Class(
  "Retrieve",
  inherit = Segment,
  
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
      # get object first
      # self$runTasks("hpc.retrieveCciaObj")
      
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
      
      # get label files
      labelFiles <- self$funParams()$valueNames
      
      # add suffixes
      labelSuffixes <- unique(unlist(lapply(
        labelFiles,
        function(x) cciaObj$valueSuffixes("imLabelsFilepath", valueName = x))))
      
      if (length(labelSuffixes) > 0) {
        labelFiles <- c(
          labelFiles,
          paste(labelFiles, labelSuffixes, sep = "_")
        )
      }
      
      filesToGet <- c(
        taskDirFiles("labels", labelFiles),
        taskDirFiles("labelProps", self$funParams()$valueNames)
      )
      
      if ("retrieveMeshesOnly" %in% names(self$funParams())) {
        if (self$funParams()$retrieveMeshesOnly == TRUE) {
          filesToGet <- c()
        }
      }
      
      # retrieve meshes?
      # TODO this takes a while
      # you should consider combining meshes
      # into a scene.. ? or something else
      if ("retrieveMeshes" %in% names(self$funParams())) {
        if (self$funParams()$retrieveMeshes == TRUE) {
          filesToGet <- c(
            filesToGet,
            taskDirFiles("mesh", self$funParams()$valueNames, isDir = TRUE)
          )
        }
      }
      
      # retrieve remote files
      taskVars$fun <- list(
        remoteFiles = filesToGet,
        localDir = localDir,
        remoteDir = remoteDir,
        useCompression = TRUE,
        useArchive = TRUE
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
      # self$updateImageInfo(valueNames = self$funParams()$valueNames,
      #                      labelSuffixes = as.list(labelSuffixes))
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
