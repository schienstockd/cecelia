Copy <- R6::R6Class(
  "Copy",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "copy",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create dir
      dir.create(self$envParams()$dirs$zero)
      
      # copy file
      file.copy(
        cciaObj$oriFilepath(),
        file.path(
          self$envParams()$dirs$zero,
          sprintf("%s.ome.tiff", fileIMAGE_CONVERTED))
      )
      
      # update image information
      self$updateImageInfo()
    }
  )
)
