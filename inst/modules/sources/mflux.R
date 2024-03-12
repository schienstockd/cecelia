# Base class for Mediaflux methods
Mflux <- R6::R6Class(
  "Mflux",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "mflux"
    },
    
    # prepare Mediaflux config
    prepMfluxConfig = function(tmpdir = NULL) {
      # get access token
      fileConn <- file(self$utilsParams()$mflux$tokenfile)
      mfluxToken <- readLines(fileConn)
      close(fileConn)
      
      # prepare config
      mfluxConfig <- c(
        paste0("host=", self$utilsParams()$mflux$host),
        paste0("port=", self$utilsParams()$mflux$port),
        paste0("transport=", self$utilsParams()$mflux$transport),
        paste0("token=", mfluxToken)
      )
      
      if (is.null(tmpdir))
        tmpdir <- tempdir()

      mfluxConfigFile <- tempfile(tmpdir = tmpdir, fileext = ".tmp")
      
      fileConn <- file(mfluxConfigFile)
      writeLines(mfluxConfig, fileConn)
      close(fileConn)
      
      return(mfluxConfigFile)
    },
    
    # prepare SMB config
    prepSmbConfig = function(tmpdir = NULL) {
      # split off domain and username
      username <- stringi::stri_split_fixed(self$utilsParams()$smb$username, "/")[[1]]
      
      # prepare config
      smbConfig <- c(username[[2]], username[[1]],
                     .cciaDecrypt(self$utilsParams()$smb$password))
      
      if (is.null(tmpdir))
        tmpdir <- tempdir()
      
      smbConfigFile <- tempfile(tmpdir = tmpdir, fileext = ".tmp")
      
      fileConn <- file(smbConfigFile)
      writeLines(smbConfig, fileConn)
      close(fileConn)
      
      return(smbConfigFile)
    },
    
    # SMB address
    smbAddress = function() {
      paste0("smb:", self$utilsParams()$smb$remoteDir, self$utilsParams()$smb$remoteAddon)
    },
    
    # prepare SMB mount
    mountSMB = function() {
      # need to push config file to server to access mediaflux
      mfluxConfigFile <- self$prepMfluxConfig(
        tmpdir = file.path(self$envParams()$dirs$task, "tasks"))
      
      # push lab server credentials to HPC
      smbConfigFile <- self$prepSmbConfig(
        tmpdir = file.path(self$envParams()$dirs$task, "tasks"))
      configFiles <- c(mfluxConfigFile, smbConfigFile)
      
      # transfer configs
      funParams <- list(
        localFiles = configFiles,
        localDir = self$envParams("local")$dirs$task,
        remoteDir = self$envParams("hpc")$dirs$task,
        useCompression = FALSE
      )
      self$runTasks(c("hpc.upload"), funParams = funParams)
      
      # unlink configs
      unlink(configFiles)
      
      # start transfer
      mfluxConfigFile <- paste0(self$envParams("hpc")$dirs$task, "/tasks/", basename(mfluxConfigFile))
      smbConfigFile <- paste0(self$envParams("hpc")$dirs$task, "/tasks/", basename(smbConfigFile))
      
      # mount SMB directory
      cmd <- paste("gio mount", self$smbAddress(), "<", smbConfigFile)
      
      # prepare SMB directory
      smbServer <- stringr::str_split(self$utilsParams()$smb$remoteDir, "/")[[1]]
      smbServer <- smbServer[smbServer != ""]
      
      # get user ID
      # TODO this whole procedure show probably be a bash script
      userID <- stringr::str_trim(self$sshConnection()$sshExecute("id -u", intern = TRUE))
      
      smbDir <- paste0(
        "/run/user/", userID, "/gvfs/smb-share:server=", smbServer[[1]],
        # "/run/user/$(USER_ID)/gvfs/smb-share:server=", smbServer[[1]],
        ",share=", paste0(tolower(smbServer[[2]]), self$utilsParams()$smb$remoteAddon))
      
      # append directory
      pDir <- paste0(smbDir, "/", self$funParams()$pDir)
      
      return(list(
        cmd = cmd,
        pDir = pDir,
        mfluxConfigFile = mfluxConfigFile,
        smbConfigFile = smbConfigFile
      ))
    },
    
    # upload files
    upload = function(FilesToUpload) {
      self$initLog()
      self$writeLog("Upload")
      
      # prep config
      mfluxConfigFile <- self$prepMfluxConfig()
      
      # prepare call
      cmd <- paste(c(
        # this is unimelb specific
        file.path(cciaConf()$dirs$mfluxClients, "unimelb-mf-upload"),
        paste("--mf.config", mfluxConfigFile),
        paste("--nb-workers", self$utilsParams()$mflux$nbWorkers),
        if (self$utilsParams()$mflux$sync == TRUE) "--sync-delete-assets" else NULL,
        "--quiet",
        "--create-namespaces",
        paste("--namespace", paste0(self$utilsParams()$mflux$namespace, "/", self$utilsParams()$mflux$username)),
        FilesToUpload
      ), collapse = " ")
      
      # call mediaflux
      self$writeLog(cmd)
      handleSystem(.execSystem(cmd, intern = FALSE))
      
      # clean up
      unlink(mfluxConfigFile)
      
      self$writeLog("Done")
      self$exitLog()
    },
    
    # retrieve files
    retrieve = function(retrPID) {
      self$initLog()
      self$writeLog("Retrieve")
      
      # create destination dir
      pDir <- file.path(dirname(self$globalParams()$projectsDir))
      
      # prep config
      mfluxConfigFile <- self$prepMfluxConfig()
      
      # prepare call
      cmd <- paste(c(
        # TODO this is unimelb specific
        file.path(cciaConf()$dirs$mfluxClients, "unimelb-mf-download"),
        paste("--mf.config", mfluxConfigFile),
        paste("--nb-workers", self$utilsParams()$mflux$nbWorkers),
        "--quiet",
        "--overwrite",
        paste("-o", pDir),
        paste(self$utilsParams()$mflux$namespace, self$utilsParams()$mflux$username, retrPID, sep = "/")
      ), collapse = " ")
      
      # call mediaflux
      self$writeLog(cmd)
      handleSystem(.execSystem(cmd, intern = FALSE))
      
      # clean up
      unlink(mfluxConfigFile)
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)