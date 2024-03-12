BackupProject <- R6::R6Class(
  "BackupProject",
  inherit = Mflux,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "backupProject",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$initLog()
      self$writeLog("Backup")
      
      # prepare SMB
      smbConfig <- self$mountSMB()
      retrPID <- self$globalParams()$pID
      
      # check that the module is loaded
      # TODO this is unimelb specific
      cmd <- paste(smbConfig$cmd, "module load unimelb-mf-clients", sep = ";")
      mfluxBin <- "unimelb-mf-download"
      
      # prepare call
      cmd <- paste(cmd, paste(c(
        # TODO this is unimelb specific
        mfluxBin,
        paste("--mf.config", smbConfig$mfluxConfigFile),
        # TODO this will run on a login node
        paste("--nb-workers", self$utilsParams()$mflux$nbWorkers),
        # paste("--nb-workers 1"),
        "--quiet",
        "--overwrite",
        paste("-o", smbConfig$pDir),
        paste(self$utilsParams()$mflux$namespace, self$utilsParams()$mflux$username, retrPID, sep = "/")
      ), collapse = " "), sep = ";")
      
      # clean up
      cmd <- paste(cmd, paste("rm", smbConfig$mfluxConfigFile, smbConfig$smbConfigFile), sep = ";")
      
      # unmount
      cmd <- paste(cmd, paste("gio mount -u", self$smbAddress()), sep = ";")
      
      handleSystem(self$sshConnection()$sshExecute(
        cmd, intern = FALSE, outputFile = self$getTaskLogFile()))
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
