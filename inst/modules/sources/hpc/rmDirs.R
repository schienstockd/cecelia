RmDirs <- R6::R6Class(
  "RmDirs",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "rmDirs",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$initLog()
      self$writeLog("Remove directories")
      self$writeLog(is.null(self$sshConnection()))
      
      # check and wait for file to be created
      self$sshConnection()$sshExecute(
        # delete files
        paste(
          sprintf("test -d '%1$s' && rm -fr '%1$s'", self$funParams()$dirs),
          collapse = "; "
          ), intern = FALSE
        )
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
