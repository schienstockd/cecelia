source(file.path(
  cfg$tasks$sources, "hpc.R")
)

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
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # update libraries
      self$sshConnection()$sshExecute(
        paste(
          sprintf("cd %s", dirname(self$envParams("hpc")$dirs$zero)),
          # remove all transferred images from user directory
          "rm ./*/ImageToImport*",
          sep = ";"
          # TODO this should output on console
          # but does not appear in sink(logfile)
          ), intern = FALSE, outputFile = self$getTaskLogFile())
    }
  )
)