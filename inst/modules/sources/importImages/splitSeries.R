SplitSeries <- R6::R6Class(
  "SplitSeries",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "splitSeries",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Split series")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get root
      oriFilename <- cciaObj$oriFilepath()
      rootDir <- cciaObj$persistentObjectDirectory(root = TRUE)
      zeroRootDir <- cciaObj$persistentObjectDirectory(root = TRUE, zero = TRUE)
      
      # create new images
      newImages <- list()
      newUIDs <- list()
      
      # get number of series
      cmd <- paste(
        sprintf("cd \"%s\";", dirname(oriFilename)),
        file.path(sprintf("%s", cciaCondaPath()), "bin", "showinf"),
        basename(oriFilename),
        "-nopix",
        "| grep 'Series count'"
      )
      
      self$writeLog("Get series numbers")
      
      res <- .execSystem(cmd)
      seriesNum <- as.numeric(stringr::str_extract(res, "(?<=Series count = )[0-9]+"))
      
      self$writeLog(paste(">>", seriesNum))
      
      for (i in seq(0, seriesNum - 1)) {
        curParams <- list(
          Name = paste(cciaObj$getCciaName(),
                       sprintf("[series %d]", i)),
          Type = "Image",
          Class = "CciaImage",
          Meta = list(
            "oriFilepath" = cciaObj$oriFilepath(),
            "imSeries" = i
          )
        )
        
        # init
        newUID <- genUID(cciaConf()$images$lenUID)
        newImage <- CciaImage$new(
          file.path(
            rootDir, newUID, cecelia:::CCID_STATE_FILE
          ),
          newUID, initParams = curParams, retrieveState = FALSE)
        
        # add origin image as attribute
        newImage$addCciaAttr("originUID", cciaObj$getUID(), invalidate = FALSE)
        newImage$addCciaAttr("numImage", i, invalidate = FALSE)
        
        # add to list
        newImages <- append(newImages, newImage)
        newUIDs <- c(newUIDs, newUID)
      }
      
      self$writeLog("Done, save to set")
      
      # init split set
      splitSet <- initCciaObject(
        file.path(
          rootDir, self$funParams()$splitSetID
        ), initReactivity = FALSE, initTransaction = TRUE, waitForRelease = TRUE
      )
      
      # remove images previously generated from this image
      # if (self$funParams()$removePrevious == TRUE) {
      splitSet$removeCciaObjects(
        splitSet$cciaObjectsByAttr("originUID", cciaObj$getUID())
      )
      # }
      
      self$writeLog("Add images to set")
      
      # add to set
      splitSet$addCciaObjects(newImages)
      
      # save training set
      splitSet$saveState()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
