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
          sprintf("cd %s", cciaConf()$hpc$dirs$cecelia),
          "sh ./scripts/hpc/slurm/updateUserLibraries.sh",
          # submit job to install python libraries
          # otherwise it will be killed due to 4GB memory limit on login node
          sprintf("sbatch --wait -o %1$s.log -e %1$s.log ./scripts/hpc/slurm/installConda.slurm",
                  file.path(self$envParams()$dirs$task, "updateUserLibraries")),
          sep = ";"
          # TODO this should output on console
          # but does not appear in sink(logfile)
          ), intern = FALSE, outputFile = self$getTaskLogFile())
    }
  )
)
