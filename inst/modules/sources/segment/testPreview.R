TestPreview <- R6::R6Class(
  "TestPreview",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "testPreview",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$initLog()
      self$writeLog("Start test")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # call python
      self$pyScript("test_preview")
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
