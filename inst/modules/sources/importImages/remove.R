Remove <- R6::R6Class(
  "Remove",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "remove",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$initLog()
      
      valueName <- self$funParams()$valueName
      newDefault <- self$funParams()$newDefault
      
      # get image path
      imPath <- cciaObj$imFilepath(valueName = valueName)
      
      # remove content from HPC
      if (self$funParams()$removeHPC == TRUE) {
        # get hpc path
        hpcPath <- paste0(
          self$envParams("hpc")$dirs$zero, "/",
          basename(imPath))
        
        funParams <- list(dirs = c(hpcPath))
        
        # assuming the image is a directory
        self$writeLog(paste("Remove HPC", hpcPath))
        self$runTasks(c("hpc.rmDirs"), funParams = funParams)
      }
      
      # remove content
      self$writeLog(paste("Remove", imPath))
      unlink(imPath, recursive = TRUE)
      
      # reset image information
      cciaObj$setImFilepath(NULL, valueName = valueName)
      cciaObj$setImChannelNames(NULL, valueName = valueName)
      
      # set new default
      # TODO there should be a method for this
      cciaObj$setImFilepath(
        basename(cciaObj$imFilepath(valueName = newDefault)), valueName = newDefault)
      cciaObj$setImChannelNames(
        cciaObj$imChannelNames(valueName = newDefault), valueName = newDefault)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # save back
      cciaObj$saveState()
    }
  )
)
