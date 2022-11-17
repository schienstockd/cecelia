RmFiles <- R6::R6Class(
  "RmFiles",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "rmFiles",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$initLog()
      self$writeLog("Remove files")
      self$writeLog(is.null(self$sshConnection()))
      
      # check and wait for file to be created
      self$sshConnection()$sshExecute(
        # delete files
        paste(
          sprintf("test -f '%1$s' && rm '%1$s'", self$funParams()$files),
          collapse = "; "
          ), intern = FALSE
        )
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
