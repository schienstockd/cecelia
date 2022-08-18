ResetImageInfo <- R6::R6Class(
  "ResetImageInfo",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "resetImageInfo",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
    }
  )
)
