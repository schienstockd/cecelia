source(file.path(
  cfg$tasks$sources, "hpc.R")
)

UpdateUserLibraries <- R6::R6Class(
  "UpdateUserLibraries",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "updateUserLibraries",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # update libraries
      self$sshConnection()$sshExecute(
        paste(
          sprintf("cd %s", cfg$hpc$dirs$cecelia),
          "sh ./hpc-setup/init-HPC-packages.sh",
          sep = ";"
          # TODO this should output on console
          # but does not appear in sink(logfile)
          ), intern = FALSE, outputFile = self$getTaskLogFile())
    }
  )
)