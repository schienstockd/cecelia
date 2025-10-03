ResetImageInfo <- R6::R6Class(
  "ResetImageInfo",
  inherit = Launchpad,
  
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
      # Dummy function
    }
  )
)
