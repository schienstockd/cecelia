CreateCombinedImage <- R6::R6Class(
  "CreateCombinedImage",
  inherit = Pixcl,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "createCombinedImage",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start create combined image")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get root
      rootDir <- cciaObj$persistentObjectDirectory(root = TRUE)
      zeroRootDir <- cciaObj$persistentObjectDirectory(root = TRUE, zero = TRUE)
      
      # create new image
      newUID <- genUID(cciaConf()$images$lenUID)
      
      imParams <- list(
        Name = paste("Combo Image", self$funParams()$imName),
        Type = "Image",
        Class = "CciaImage",
        Meta = list(
          "oriFilepath" = NULL
        )
      )
      
      # generate image
      newImage <- CciaImage$new(
        file.path(
          rootDir, newUID, cecelia:::CCID_STATE_FILE
        ),
        newUID, initParams = imParams,
        retrieveState = FALSE)
      
      # select random uIDs
      uIDs <- self$funParams()$uIDs
      
      if (self$funParams()$numRandImages > 0) {
        numImages <- if (self$funParams()$numRandImages <= length(uIDs))
          self$funParams()$numRandImages
        else
          length(uIDs)
        
        uIDs <- sample(uIDs, numImages)
      }
      
      self$writeLog(sprintf(">> Use %d images from %d requested",
                            length(uIDs), self$funParams()$numRandImages))
      
      # add new filepath
      newFilepath <- "ccidImage.zarr"
      attr(newFilepath, "partOf") <- uIDs
      newImage$setImFilepath(newFilepath)
      
      # add origin image as attribute
      newImage$addCciaAttr("originuID", cciaObj$getUID(), invalidate = FALSE)
      newImage$addCciaAttr("numImages", length(uIDs), invalidate = FALSE)
      
      newImage$saveState()
      
      # init training set
      trainingSet <- initCciaObject(
        file.path(
          rootDir, self$funParams()$trainingSetID
        ), initReactivity = FALSE
      )
      
      # # remove images previously generated crops from this image
      # trainingSet$removeCciaObjects(
      #   trainingSet$cciaObjectsByAttr("originUID", cciaObj$getUID())
      # )
      
      # add to set
      trainingSet$addCciaObjects(c(newImage))
      trainingSet$saveState()
      
      self$writeLog(cciaObj$cciaObjects(uIDs = uIDs)[[1]]$imFilepath())
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        zeroRootDir = zeroRootDir,
        # assuming they all come from the same source
        # use the value from the first image
        imSourceName = basename(cciaObj$cciaObjects(uIDs = uIDs)[[1]]$imFilepath()),
        imPath = newImage$imFilepath(),
        uIDs = uIDs,
        crop = self$funParams()$crop,
        numCols = self$funParams()$numCols,
        normaliseImage = self$funParams()$normaliseImage,
        normPercentile = self$funParams()$normPercentile
      )
      
      # call python
      self$pyScript("create_combined_image", params)
      
      # add channels after creating image
      # TODO this assumes it is a set that is called
      newImage$setImChannelNames(cciaObj$cciaObjects(
        uIDs = self$funParams()$uIDs
      )[[1]]$imChannelNames(rmAttr = TRUE))
      
      newImage$saveState()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
