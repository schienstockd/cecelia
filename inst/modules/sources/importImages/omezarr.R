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
      
      # copy to temp if needed
      if ("copyToTmp" %in% names(self$funParams()) && self$funParams()$copyToTmp == TRUE) {
        # add special type files
        extraFiles <- c()
        # if (self$funParams()$specialType == "tenxXenium")
        #   extraFiles <- c("transcripts.csv.gz")
        
        # get files to copy
        filesToCopy <- prepFilelistToSync(
          imPathIn,
          fileIMAGE_TO_IMPORT,
          isSequence = any(self$funParams()$isSequence, self$funParams()$is3P),
          extraFiles = extraFiles
        )
        
        if (length(filesToCopy$files) > 1) {
          # make directory
          tmpFilepath <- tempfile()
          dir.create(tmpFilepath)
          
          toFiles <- file.path(tmpFilepath, basename(filesToCopy$names))
          file.copy(filesToCopy$files, toFiles)
          imPathIn <- toFiles[[1]]
        } else {
          tmpFilepath <- tempfile(fileext = paste0(".", tools::file_ext(imPathIn)))
          
          file.copy(imPathIn, tmpFilepath)
          imPathIn <- tmpFilepath
        }
        
        self$writeLog(paste(">> Copy to", tmpFilepath))
      }
      
      imPathInPattern <- "\"%s\""
      if (self$callingEnv() == "hpc") {
        # get basename of modified image
        if ("modified" %in% attributes(imPathIn) && attr(imPathIn, "modified") == TRUE) {
          imPathIn <- file.path(
            self$envParams()$dirs$zero, basename(imPathIn))
        } else if ("is3P" %in% names(self$funParams()) && self$funParams()$is3P == TRUE) {
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
          stackDim = self$funParams()$stackDim,
          seqREXP = self$funParams()$seqREXP
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
          if ("bioformats2raw" %in% names(cciaConf()$dirs))
            file.path(sprintf("%s", cciaConf()$dirs$bioformats2raw), "bin", "bioformats2raw")
          else
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
      
      # remove temporary file
      if ("copyToTmp" %in% names(self$funParams()) && self$funParams()$copyToTmp == TRUE) {
        self$writeLog(paste(">> Remove", tmpFilepath))
        unlink(tmpFilepath, recursive = TRUE)
      }
      
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
