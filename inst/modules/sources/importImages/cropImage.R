CropImage <- R6::R6Class(
  "CropImage",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cropImage",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create dir
      self$initLog()
      self$writeLog(">> Crop image")
      
      # get im path
      imPathIn <- file.path(
        self$envParams()$dirs$zero,
        basename(cciaObj$imFilepath(valueName = self$funParams()$valueName)))
  
      imPathOut = file.path(
        self$envParams()$dirs$zero,
        "ccidCropped.zarr"
      )
      
      # prepare params
      params <- list(
        imPathIn = imPathIn,
        imPathOut = imPathOut,
        cropCoords = cropCoords
      )
      
      # call python
      self$pyScript("crop_image", params)
      
      # set new image to import as attribute
      attr(imPathIn, "modified") <- imPathOut
      
      self$writeLog(paste(">> OUT", imPathOut))
      
      # set file for original
      cciaObj$setOriFilepath(imPathIn)
      cciaObj$saveState()
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
