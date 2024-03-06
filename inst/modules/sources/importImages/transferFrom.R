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
      
      # add special type files
      extraFiles <- c()
      if (self$funParams()$specialType == "tenxXenium")
        extraFiles <- c("transcripts.csv.gz")
      
      # get files to copy
      filesToCopy <- prepFilelistToSync(
        cciaObj$oriFilepath(),
        fileIMAGE_TO_IMPORT,
        isSequence = self$funParams()$isSequence,
        extraFiles = extraFiles
      )
      
      self$writeLog(smbCmd)
      self$writeLog("Transfer from SMB server")
      # self$writeLog(unlist(filesToCopy))
      
      # create directory
      self$writeLog(">> create directory")
      self$writeLog(smbCmd)
      handleSystem(self$sshConnection()$sshExecute(smbCmd))
      
      self$writeLog(">> GET command")
      self$writeLog(
        sprintf(
          "smbclient %s -U %s -c 'prompt OFF; recurse ON; mask \\\"\\\"; cd \\\"%s\\\"; cd \\\"%s\\\"'",
          self$utilsParams()$smb$remoteDir,
          self$utilsParams()$smb$username,
          self$utilsParams()$smb$remoteAddon,
          getDir
          # self$envParams(remoteTo)$dirs$zero
          # cmdMGET, cmdRename
        ))
      
      # batch files into 20 files
      batchSize <- 500
      nFiles <- length(filesToCopy$files)
      
      for (i in seq(ceiling(nFiles/batchSize))) {
        iStart <- (i-1) * batchSize
        iStop <- i * batchSize
        
        if (iStop > nFiles)
          iStop <- nFiles
        
        self$writeLog(paste("> Transfer files", iStart, "-", iStop))
        
        # create mget
        cmdMGET <- paste(sprintf(
          "mget %s",
          sprintf('\\\"%s\\\"', basename(filesToCopy$files[iStart:iStop])), collapse = " "
        ), collapse = "; ")
        
        # create rename
        cmdRename <- ""
        if (self$funParams()$isSequence == FALSE) {
          cmdRename <- paste(sprintf(
            "mv \\\"%s\\\" \\\"%s\\\"",
            paste(self$envParams(remoteTo)$dirs$zero, basename(filesToCopy$files[iStart:iStop]), sep = "/"),
            paste(self$envParams(remoteTo)$dirs$zero, basename(filesToCopy$names[iStart:iStop]), sep = "/")
          ), collapse = "; ")
        }
        
        # TODO credentials should be in temporary file - not in command!
        # copy
        handleSystem(self$sshConnection()$sshExecute(sprintf(
          "echo $'%s' | smbclient %s -U %s -c 'prompt OFF; recurse ON; mask \\\"\\\"; cd \\\"%s\\\"; cd \\\"%s\\\"; lcd %s; %s'; %s",
          .prepForBash(.cciaDecrypt(self$utilsParams()$smb$password)),
          self$utilsParams()$smb$remoteDir,
          self$utilsParams()$smb$username,
          self$utilsParams()$smb$remoteAddon,
          getDir, self$envParams(remoteTo)$dirs$zero,
          cmdMGET, cmdRename
        )))
      }
      
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
