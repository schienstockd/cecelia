CleanupTransferredImages <- R6::R6Class(
  "CleanupTransferredImages",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cleanupTransferredImages",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # clean up images
      # Renamed files
      self$sshConnection()$sshExecute(
        paste(
          sprintf("cd %s", dirname(self$envParams("hpc")$dirs$zero)),
          # remove all transferred images from user directory
          "rm ./*/ImageToImport*",
          sep = ";"
          # TODO this should output on console
          # but does not appear in sink(logfile)
          ), intern = FALSE, outputFile = self$getTaskLogFile())
      
      # TIF files
      # TODO should this include other file types?
      self$sshConnection()$sshExecute(
        paste(
          sprintf("cd %s", dirname(self$envParams("hpc")$dirs$zero)),
          # remove all transferred TIF images from user directory
          "rm ./*/*.tif",
          sep = ";"
          ), intern = FALSE, outputFile = self$getTaskLogFile())
    }
  )
)
