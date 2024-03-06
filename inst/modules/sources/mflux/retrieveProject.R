RetrieveProject <- R6::R6Class(
  "RetrieveProject",
  inherit = Mflux,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "retrieveProject",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # retrieve whole project folder
      self$retrieve(
        self$funParams()$retrPID,
        pDir = if ("pDir" %in% names(self$funParams()))
          self$funParams()$pDir else NULL
        )
    }
  )
)
