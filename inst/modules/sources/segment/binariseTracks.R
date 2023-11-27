# Binarise tracks
BinariseTracks <- R6::R6Class(
  "BinariseTracks",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "binariseTracks",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start tracks segmentation")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      valueName <- self$funParams()$valueName
      binName <- paste0(valueName, ".", "tracks")
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = file.path(
          self$envParams()$dirs$zero,
          basename(cciaObj$imFilepath())
        ),
        pops = self$funParams()$pops
      )
      
      # save pops before call
      cciaObj$savePopMap("live", includeFiltered = TRUE)
      cciaObj$savePops("live", purge = TRUE, includeFiltered = TRUE)
      
      # call python
      self$pyScript("binarise_tracks", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(valueName = binName)
    }
  )
)
