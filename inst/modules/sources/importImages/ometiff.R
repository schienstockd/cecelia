Ometiff <- R6::R6Class(
  "Ometiff",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "ometiff",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$resetImageInfo()
      
      # call pre import
      self$preImport()
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # define filepaths
      imPathIn <- cciaObj$oriFilepath(modified = TRUE, revertToOri = TRUE)
      
      self$initLog()
      
      imPathInPattern <- "\"%s\""
      if (self$callingEnv() == "hpc") {
        # get basename of modified image
        if ("modified" %in% attributes(imPathIn) && attr(imPathIn, "modified") == TRUE) {
          imPathIn <- file.path(
            self$envParams()$dirs$zero, basename(imPathIn))
        } else {
          imPathIn <- file.path(
            self$envParams()$dirs$zero, "ImageToImport.*")
          
          # allow wildcard completion
          imPathInPattern <- "%s"
        }
      }
      
      self$writeLog(paste("Import", imPathIn))
      
      imPathOut <- file.path(
        self$envParams()$dirs$zero,
        sprintf("%s.ome.tiff", fileIMAGE_CONVERTED)
      )
      
      self$writeLog(paste("Save as", imPathOut))
      
      # use pyramid?
      pyramidScaleStr <- c()
      if (self$funParams()$pyramidScale > 0) {
        pyramidScaleStr <- paste(
          "-tilex 512 -tiley 512 -noflat",
          sprintf("-pyramid-resolutions %d", self$funParams()$pyramidScale),
          sprintf("-pyramid-scale %d", self$funParams()$pyramidScale)
        )
      }
      
      cmd <- paste(
        sprintf("cd \"%s\";", self$envParams()$dirs$zero),
        # file.path(sprintf("%s", cciaConf()$dirs$bftools), "bfconvert"),
        file.path(sprintf("%s", cciaCondaPath()), "bin", "bfconvert"),
        paste(pyramidScaleStr, collapse = " "),
        "-bigtiff",
        "-overwrite",
        "-compression zlib",
        sprintf(imPathInPattern, imPathIn),
        sprintf("\"%s\"", imPathOut)
      )
      
      self$writeLog(paste("EXEC", cmd))
      handleSystem(.execSystem(cmd))
      
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
