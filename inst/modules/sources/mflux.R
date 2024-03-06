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
    retrieve = function(retrPID, pDir = NULL, mfluxConfigFile = NULL, smbConfigFile = NULL) {
      self$initLog()
      self$writeLog("Retrieve")
      smbMounted <- NULL
      
      # mount volumes for download?
      if (!is.null(smbConfigFile) && self$taskEnv() == "hpc") {
        # mount SMB directory
        smbAddress <- paste0("smb:", self$utilsParams()$smb$remoteDir, self$utilsParams()$smb$remoteAddon)
        cmd <- paste("gio mount", smbAddress, "<", smbConfigFile)
        
        self$writeLog(cmd)
        handleSystem(.execSystem(cmd, intern = FALSE))
        
        smbMounted <- smbAddress
        unlink(smbConfigFile)
        
        # prepare SMB directory
        userID <- .execSystem("id -u", intern = TRUE)
        smbServer <- stringr::str_split(self$utilsParams()$smb$remoteDir, "/")[[1]]
        smbServer <- smbServer[smbServer != ""]
        
        smbDir <- paste0(
          "/run/user/", userID, "/gvfs/smb-share:server=", smbServer[[1]],
          ",share=", paste0(smbServer[[2]], self$utilsParams()$smb$remoteAddon))
        
        # append directory
        pDir <- paste0(smbDir, "/", pDir)
      } else {
        # create destination dir
        if (is.null(pDir))
          pDir <- file.path(dirname(self$globalParams()$projectsDir))
      }
      
      # prep config
      if (is.null(mfluxConfigFile))
        mfluxConfigFile <- self$prepConfig()
      
      # check that the module is loaded
      # TODO this is unimelb specific
      if (self$taskEnv() == "hpc") {
        handleSystem(.execSystem("module load unimelb-mf-clients"))
        mfluxBin <- "unimelb-mf-download"
      } else {
        mfluxBin <- file.path(cciaConf()$dirs$mfluxClients, "unimelb-mf-download")
      }
      
      # prepare call
      cmd <- paste(c(
        # TODO this is unimelb specific
        mfluxBin,
        paste("--mf.config", mfluxConfigFile),
        paste("--nb-workers", self$utilsParams()$mflux$nbWorkers),
        "--quiet",
        "--overwrite",
        paste("-o", pDir),
        paste(self$utilsParams()$mflux$namespace, self$utilsParams()$mflux$username, retrPID, sep = "/")
      ), collapse = " ")
      
      # call mediaflux
      self$writeLog(cmd)
      # handleSystem(.execSystem(cmd, intern = FALSE))
      
      # clean up
      unlink(mfluxConfigFile)
      
      # unmount
      if (!is.null(smbMounted))
        handleSystem(.execSystem(paste("gio mount -u ", smbMounted), intern = FALSE))
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)