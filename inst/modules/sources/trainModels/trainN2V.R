TrainN2V <- R6::R6Class(
  "TrainN2V",
  inherit = TrainModels,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "trainN2V",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Train images with n2v")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
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
      
      # get uIDs from set for training
      trainingSet <- initCciaObject(
        file.path(
          rootDir, self$funParams()$trainingSetID
        ), initReactivity = FALSE, initTransaction = TRUE, waitForRelease = TRUE
      )
      
      trainUIDs <- names(cciaObj$cciaObjects())
      
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
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
