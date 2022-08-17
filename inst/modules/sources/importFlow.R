# Base class for import methods
ImportFlow <- R6::R6Class(
  "ImportFlow",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "importFlow"
    },
    
    # run pre import functions
    preImport = function() {
      # create dir
      dir.create(self$envParams()$dirs$zero)
    },
    
    # reset image information
    resetImageInfo = function() {
    },
    
    # update image information after conversion
    updateImageInfo = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get converted image filename
      imFilepath <- Sys.glob(file.path(
        self$envParams()$dirs$zero,
        paste0(fileIMAGE_CONVERTED, "*")))
      
      # set filename
      cciaObj$setImFilepath(basename(imFilepath))
      
      # save object
      cciaObj$saveState()
    }
  )
)