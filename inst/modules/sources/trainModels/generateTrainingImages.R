GenerateTrainingImages <- R6::R6Class(
  "GenerateTrainingImages",
  inherit = TrainModels,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "generateTrainingImages",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start generating training images")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # The user has to create a set to which to save all training images
      # it could be that the user would like to pool training images
      # from different sets together, so it might be better to give,
      # her the choice where to save this
      # then.. we need to add images to that set before starting the image
      # generation. Create four images to start with. 
      # The ID of the set will be passed to the function parameters
      
      self$writeLog(paste(
        "--Training",
        self$funParams()$trainingSetID,
        "--num",
        self$funParams()$numTrainingImages,
        "--source",
        self$funParams()$imSource,
        "--channels",
        self$funParams()$channels,
        "--crop",
        paste(self$funParams()$crop, collapse = ", "),
        "--mip",
        self$funParams()$maximumProjection
      ))
      
      # get root
      rootDir <- cciaObj$persistentObjectDirectory(root = TRUE)
      zeroRootDir <- cciaObj$persistentObjectDirectory(root = TRUE, zero = TRUE)
      
      # create new images
      newImages <- list()
      newUIDs <- list()
      for (i in seq(self$funParams()$numTrainingImages)) {
        curParams <- list(
          Name = paste(cciaObj$getCciaName(),
                       sprintf("[crop %d]", i)),
          Type = "Image",
          Class = "CciaImage",
          Meta = list(
            "oriFilepath" = cciaObj$oriFilepath()
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
      
      # init training set
      trainingSet <- initCciaObject(
        file.path(
          rootDir, self$funParams()$trainingSetID
        ), initReactivity = FALSE, initTransaction = TRUE, waitForRelease = TRUE
      )
      
      # remove images previously generated crops from this image
      if (self$funParams()$removePrevious == TRUE) {
        trainingSet$removeCciaObjects(
          trainingSet$cciaObjectsByAttr("originUID", cciaObj$getUID())
        )
      }
      
      # add to set
      trainingSet$addCciaObjects(newImages)
      
      # save training set
      trainingSet$saveState()
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath(self$funParams()$imSource))
        ),
        trainingSetPath = trainingSet$persistentObjectDirectory(),
        trainingImagePaths = file.path(zeroRootDir, newUIDs, "ccidImage.zarr"),
        channels = sapply(self$funParams()$channels, as.integer),
        crop = lapply(self$funParams()$crop, as.numeric),
        maximumProjection = self$funParams()$maximumProjection
      )
      
      # call python
      self$pyScript("generate_training_images", params)
      
      # set image paths and save back
      for (x in newImages) {
        x$setImFilepath("ccidImage.zarr")
        x$setImChannelNames(cciaObj$imChannelNames()[params$channels + 1])
        
        x$saveState()
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
