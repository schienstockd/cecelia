Omezarr <- R6::R6Class(
  "Omezarr",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "omezarr",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$resetImageInfo()
      
      # call pre import
      self$preImport()
      
      # get object
      cciaObj <- self$cciaTaskObject(forceReload = TRUE)
      
      self$initLog()
      
      # define filepaths
      imPathIn <- cciaObj$oriFilepath(modified = TRUE, revertToOri = TRUE)
      
      imPathInPattern <- "\"%s\""
      if (self$callingEnv() == "hpc") {
        # get basename of modified image
        if ("modified" %in% attributes(imPathIn) && attr(imPathIn, "modified") == TRUE) {
          imPathIn <- file.path(
            self$envParams()$dirs$zero, basename(imPathIn))
        } else if (self$funParams()$isSequence == FALSE) {
          # revert to Import image only if not sequence
          # otherwise take the original file name
          imPathIn <- file.path(
            self$envParams()$dirs$zero, "ImageToImport.*")
          
          # allow wildcard completion
          imPathInPattern <- "%s"
        }
      }
      
      self$writeLog(paste("Import", imPathIn))
      
      imPathOut <- file.path(
        self$envParams()$dirs$zero,
        sprintf("%s.ome.zarr", fileIMAGE_CONVERTED)
      )
      
      self$writeLog(paste("Save as", imPathOut))
      
      # remove previous directory
      unlink(imPathOut, recursive = TRUE)
      
      # use pyramid?
      pyramidScaleStr <- ""
      if (self$funParams()$pyramidScale > 0) {
        pyramidScaleStr <- sprintf("--resolutions %d", self$funParams()$pyramidScale)
      }
      
      # check whether image is a sequence
      if (self$funParams()$isSequence) {
        self$writeLog(">> Create image from sequence")
        
        # create params
        params <- list(
          imPath = imPathIn,
          zarrPath = imPathOut,
          isStacked = self$funParams()$isStacked,
          nscales = self$funParams()$pyramidScale,
          skipTiles = self$funParams()$skipTiles,
          physicalStackScale = self$funParams()$physicalStackScale,
          stackDim = self$funParams()$stackDim
        )
        
        # call python
        self$pyScript("import_from_sequence", params)
      } else {
        # use custom dimension order?
        dimOrderStr <- ""
        if (self$funParams()$dimOrder != "") {
          dimOrderStr <- sprintf("--dimension-order %s", self$funParams()$dimOrder)
        }
        
        cmd <- paste(
          # sprintf("cd \"%s\";", self$envParams()$dirs$zero),
          # go to file directory
          sprintf("cd \"%s\";", dirname(imPathIn)),
          # file.path(sprintf("%s", cciaConf()$dirs$bioformats2raw), "bin", "bioformats2raw"),
          file.path(sprintf("%s", cciaCondaPath()), "bin", "bioformats2raw"),
          sprintf(imPathInPattern, basename(imPathIn)),
          sprintf("\"%s\"", imPathOut),
          pyramidScaleStr,
          dimOrderStr
        )
        
        # add blosc library if needed
        # https://github.com/glencoesoftware/bioformats2raw
        if (cciaConf()$dirs$blosc != "") {
          cmd <- paste(
            c(
              sprintf(
                "export JAVA_OPTS='-Djna.library.path=%s'",
                cciaConf()$dirs$blosc
              ),
              cmd
            ),
            collapse = "; "
          )
        }
        
        self$writeLog(paste("EXEC", cmd))
        handleSystem(.execSystem(cmd))
      }
      
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
