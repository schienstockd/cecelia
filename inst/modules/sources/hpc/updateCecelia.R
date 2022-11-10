UpdateCecelia <- R6::R6Class(
  "UpdateCecelia",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "updateCecelia",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # update libraries
      self$sshConnection()$sshExecute(
        paste(
          sprintf("cd %s", cciaConf()$hpc$dirs$cecelia),
          "sh ./scripts/hpc/slurm/updateCecelia.sh",
          sep = ";"
          # TODO this should output on console
          # but does not appear in sink(logfile)
          ), intern = FALSE, outputFile = self$getTaskLogFile())
    }
  )
)
