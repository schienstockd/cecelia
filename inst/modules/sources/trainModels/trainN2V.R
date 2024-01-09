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
      
      # get im source from first image
      # TODO this assumes the same name for all images
      imSource <- basename(cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)[[1]]$imFilepath(self$funParams()$imSource))
      
      # prepare params
      params <- list(
        valueName = self$funParams()$valueName,
        taskDir = self$envParams()$dirs$task,
        zeroDir = cciaObj$persistentObjectDirectory(root = TRUE, zero = TRUE),
        imSource = imSource,
        uIDs = self$funParams()$uIDs,
        patchXY = self$funParams()$patchXY,
        patchZ = self$funParams()$patchZ,
        trainEpochs = self$funParams()$trainEpochs,
        modelDir = file.path(
          cciaObj$persistentObjectDirectory(uID = cecelia:::CCID_IMAGE_COLLECTION), 
          "models", "n2v"),
        modelName = self$funParams()$modelName,
        modelDesc = self$funParams()$modelDesc,
        modelAuthors = self$funParams()$modelAuthors
      )
      
      # call python
      self$pyScript("train_n2v", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
