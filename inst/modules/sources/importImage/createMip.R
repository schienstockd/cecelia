source(file.path(
  cfg$tasks$sources, "import.R")
)

CreateMip <- R6::R6Class(
  "CreateMip",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "createMip",
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
      
      self$writeLog(">> Create MIP")
      
      # get im path
      if (self$callingEnv() == "hpc") {
        imPathIn <- Sys.glob(file.path(
          self$envParams()$dirs$zero, "ImageToImport.*"))
      } else {
        imPathIn <- cciaObj$oriFilepath()
      }
      
      # set out
      imPathOut <- file.path(
        self$envParams()$dirs$zero, "MipToImport.tif")
      
      # prepare params
      params <- list(
        imPathIn = imPathIn,
        imPathOut = imPathOut,
        fijiPath = cfg$imagej$path,
        scriptsPath = cfg$imagej$scripts
      )
      
      # call python
      self$pyScript("createMIP", params)
      
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
