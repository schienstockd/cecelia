RegisterImages <- R6::R6Class(
  "RegisterImages",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "registerImages",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get uIDs
      uIDs <- NULL
      if ("uIDs" %in% names(self$funParams())) {
        uIDs <- self$funParams()$uIDs
      }
      
      # reset image information
      self$resetImageInfo(valueName = "registered", uID = uIDs[[0]])
      
      self$initLog()
      self$writeLog("Start register and transformation")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get image paths
      imPaths <- lapply(
        cciaObj$cciaObjects(uIDs = uIDs),
        function(x) x$imFilepath(valueName = "default"))
      
      # convert registration channels to integers
      regChannels <- unname(lapply(
        cciaObj$cciaObjects(uIDs = uIDs),
        function(x) unname(which(cciaObj$imChannelNames() == self$funParams()$regChannel)) - 1))
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        fixedImPath = file.path(
          self$envParams()$dirs$zero,
          basename(imPaths[[0]])
        ),
        imPaths = file.path(
          self$envParams()$dirs$zero,
          basename(unlist(imPaths))
        ),
        imRegPath = file.path(
          self$envParams()$dirs$zero,
          "ccidRegistered.zarr"
        ),
        regChannels = regChannels,
        doFftInitialization = self$funParams()$doFftInitialization,
        doAffine2d = self$funParams()$doAffine2d,
        doAffine3d = self$funParams()$doAffine3d,
        ignoreSpacing = self$funParams()$ignoreSpacing,
        sigma = self$funParams()$sigma,
        autoMask = self$funParams()$autoMask,
        samplesPerParameter = self$funParams()$samplesPerParameter,
        expand = if (self$funParams()$expand > 0) self$funParams()$expand else NULL
      )
      
      # call python
      self$pyScript("register_images", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information for fixed image
      self$updateImageInfo(
        filename = "ccidRegistered", valueName = "registered", uID = uIDs[[0]])
    }
  )
)
