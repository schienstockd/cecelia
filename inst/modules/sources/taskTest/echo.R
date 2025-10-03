# test class that prints the argument on command line
Echo <- R6::R6Class(
  "Echo",
  inherit = taskTest,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "echo",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$initLog()
      self$writeLog("ECHO")
      self$writeLog(self$funParams()$message)
      self$exitLog()
    }
  )
)
