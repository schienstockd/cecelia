TestConnection <- R6::R6Class(
  "TestConnection",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "testConnection",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      handleSystem(self$sshConnection()$sshExecute("exit"))
    }
  )
)
