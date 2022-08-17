source(file.path(
  cfg$tasks$sources, "launchpad.R")
)

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
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # Dummy function
    }
  )
)