NormaliseImage <- R6::R6Class(
  "NormaliseImage",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "normaliseImage",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create dir
      dir.create(self$envParams()$dirs$zero)
      self$initLog()
      
      self$writeLog(">> Normalise image")
      
      # get im path
      if (self$callingEnv() == "hpc") {
        imPathIn <- Sys.glob(file.path(
          self$envParams()$dirs$zero, "ImageToImport.*"))
      } else {
        imPathIn <- cciaObj$oriFilepath()
      }
      
      # set out
      baseImPathIn <- basename(imPathIn)
      imPathOut <- file.path(
        self$envParams()$dirs$zero, "NormToImport.tif")
      
      # prepare params
      params <- list(
        imPathIn = imPathIn,
        imPathOut = imPathOut
      )
      
      # call python
      self$pyScript("normalise_image", params)
      
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
