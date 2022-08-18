UpdateImageInfo <- R6::R6Class(
  "UpdateImageInfo",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "updateImageInfo",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # update image information
      self$updateImageInfo()
    }
  )
)
