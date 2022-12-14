ResetImageInfo <- R6::R6Class(
  "ResetImageInfo",
  inherit = GatePopulations,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "resetImageInfo",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo(gatingSet = TRUE)
    }
  )
)
