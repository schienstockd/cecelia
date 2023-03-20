TenxXenium <- R6::R6Class(
  "TenxXenium",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "tenxXenium",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create dir
      dir.create(self$envParams()$dirs$zero)
      self$initLog()
      
      self$writeLog(">> Import from 10X Xenium")
      
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
        imPathIn = imPathIn,
        imPathOut = imPathOut,
        # pyramidScale = self$funParams()$pyramidScale,
        filterValue = self$funParams()$filterValue
      )
      
      # call python
      self$pyScript("from_tenx_xenium", params)
      
      self$writeLog("Done")
      
      # update image information
      self$updateImageInfo()
    }
  )
)
