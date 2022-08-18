PopsContainedInShapes <- R6::R6Class(
  "PopsContainedInShapes",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "popsContainedInShapes",
        sep = CCID_CLASS_SEP
      )
    },

    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start detection of populations in shapes")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = cciaObj$imFilepath(),
        popType = self$funParams()$popType,
        pops = self$funParams()$pops,
        shapes = self$funParams()$shapes
      )
        
      # call python
      self$pyScript("pops_contained_in_shapes", params)
      
      # save object
      cciaObj$saveState()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
