TransferFrom <- R6::R6Class(
  "TransferFrom",
  inherit = Import,
  
  private = list(
    # transfer files from smb server
    transferFromSMB = function() {
      # get remote 'to' environment
      remoteTo <- self$funParams()$remoteTo
      
      # get object
      cciaObj <- self$cciaTaskObject()
      self$initLog()
      
      # get directory to copy from
      getDir <- dirname(stringr::str_match(
        cciaObj$oriFilepath(),
        sprintf(
          "(?<=%s).*", self$utilsParams()$smb$localDir
        )
      ))
      
      # remove leading separator
      getDir <- stringr::str_replace(
        getDir, sprintf("^\\%s", .Platform$file.sep) , "")
      
      # create directories
      smbCmd <- sprintf(
        "mkdir -p %s", self$envParams(remoteTo)$dirs$zero
      )
      
      # get files to copy
      filesToCopy <- prepFilelistToSync(
        cciaObj$oriFilepath(),
        fileIMAGE_TO_IMPORT,
        isSequence = self$funParams()$isSequence
      )
      
      self$writeLog(smbCmd)
      self$writeLog("Transfer from SMB server")
      self$writeLog(unlist(filesToCopy))
      
      # create mget
      cmdMGET <- paste(sprintf(
        "mget %s",
        sprintf('\\\"%s\\\"', basename(filesToCopy$files)), collapse = " "
      ), collapse = "; ")
      
      # create rename
      cmdRename <- paste(sprintf(
        "mv \\\"%s\\\" \\\"%s\\\"",
        paste(self$envParams(remoteTo)$dirs$zero, basename(filesToCopy$files), sep = "/"),
        paste(self$envParams(remoteTo)$dirs$zero, basename(filesToCopy$names), sep = "/")
      ), collapse = "; ")
      
      self$writeLog(">> GET command")
      self$writeLog(sprintf(
        "smbclient %s -U %s -c 'prompt OFF; recurse ON; mask \\\"\\\"; cd \\\"%s\\\"; lcd %s; %s'; %s",
        self$utilsParams()$smb$remoteDir,
        self$utilsParams()$smb$username,
        getDir, self$envParams(remoteTo)$dirs$zero,
        cmdMGET, cmdRename
      ))
      
      # build exec string
      smbCmd <- paste(
        smbCmd,
        sprintf(
          "echo $'%s' | smbclient %s -U %s -c 'prompt OFF; recurse ON; mask \\\"\\\"; cd \\\"%s\\\"; lcd %s; %s'; %s",
          .prepForBash(.cciaDecrypt(self$utilsParams()$smb$password)),
          self$utilsParams()$smb$remoteDir,
          self$utilsParams()$smb$username,
          getDir, self$envParams(remoteTo)$dirs$zero,
          cmdMGET, cmdRename
        ),
        sep = ";"
      )
      
      # exec
      handleSystem(self$sshConnection()$sshExecute(smbCmd))
      
      self$writeLog("Done")
      self$exitLog()
    }
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "transferFrom",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      if (self$funParams()$remoteFrom == "smb") {
        private$transferFromSMB()
      }
    }
  )
)
