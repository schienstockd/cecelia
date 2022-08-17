CreatePseudotime <- R6::R6Class(
  "CreatePseudotime",
  inherit = BehaviourAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "createPseudotime",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog(">> Create pseudotime")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        initRoot = self$funParams()$initRoot
      )
      
      # call python
      self$pyScript("create_pseudotime", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
