CellToLocation <- R6::R6Class(
  "CellToLocation",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellToLocation",
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
      
      self$writeLog(">> Import from cell2location")
      
      # define filepaths
      imPathIn <- cciaObj$oriFilepath(modified = TRUE, revertToOri = TRUE)
      
      if (self$callingEnv() == "hpc") {
        # get basename of modified image
        if ("modified" %in% attributes(imPathIn) && attr(imPathIn, "modified") == TRUE) {
          imPathIn <- file.path(
            self$envParams()$dirs$zero, basename(imPathIn))
        } else {
          # complete filepath
          imPathIn <- unlist(list.files(
            self$envParams()$dirs$zero,
            pattern = "ImageToImport",
            full.names = TRUE
          ))
        }
      }
      
      self$writeLog(paste("Import", imPathIn))
      
      imPathOut <- file.path(
        self$envParams()$dirs$zero,
        sprintf("%s.zarr", fileIMAGE_CONVERTED)
      )
      
      self$writeLog(paste("Save as", imPathOut))
      
      # remove previous directory
      unlink(imPathOut, recursive = TRUE)
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        adataFilepath = imPathIn,
        zarrFilepath = imPathOut,
        pyramidScale = self$funParams()$pyramidScale
      )
      
      # call python
      self$pyScript("from_cellToLocation", params)
      
      self$writeLog("Done")
      self$exitLog()
      
      # add label props information
      cciaObj$setImLabelPropsFilepath(
        paste0(self$funParams()$valueName, cciaConf()$files$ext$labelProps),
        valueName = self$funParams()$valueName
      )
      cciaObj$saveState()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
