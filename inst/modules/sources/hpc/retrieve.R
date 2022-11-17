Retrieve <- R6::R6Class(
  "Retrieve",
  inherit = Hpc,
  
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
      syncExt <- NULL
      if (self$funParamNotNull("syncExt")) {
        syncExt <- self$funParams()$syncExt
      }
      
      self$retrieve(
        self$funParams()$remoteFiles,
        self$funParams()$localDir,
        self$funParams()$remoteDir,
        syncExt = syncExt,
        useCompression = if ("useCompression" %in% names(self$funParams()))
          self$funParams()$useCompression else FALSE,
        useArchive = if ("useArchive" %in% names(self$funParams()))
          self$funParams()$useArchive else FALSE
      )
    }
  )
)
