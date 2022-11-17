Upload <- R6::R6Class(
  "Upload",
  inherit = Hpc,
  
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
      newFilename <- NULL
      if (self$funParamNotNull("newFilename")) {
        newFilename <- self$funParams()$newFilename
      }
      
      self$upload(
        self$funParams()$localFiles,
        self$funParams()$localDir,
        self$funParams()$remoteDir,
        newFilename = newFilename,
        useCompression = if ("useCompression" %in% names(self$funParams()))
          self$funParams()$useCompression else FALSE,
        useArchive = if ("useArchive" %in% names(self$funParams()))
          self$funParams()$useArchive else FALSE,
        removeFiles = if ("removeFiles" %in% names(self$funParams()))
          self$funParams()$removeFiles else FALSE
        )
    }
  )
)
