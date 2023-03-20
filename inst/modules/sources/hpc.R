# Base class for hpc methods
Hpc <- R6::R6Class(
  "Hpc",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "hpc"
    },
    
    # upload files
    upload = function(localFiles, localDir, remoteDir, 
                      newFilename = NULL, removeFiles = FALSE,
                      isSequence = FALSE, extraFiles = list(), ...) {
      newFilenames <- NULL
      
      # prepare file list 
      if (!is.null(newFilename)) {
        filelist <- prepFilelistToSync(
          localFiles, newFilename, isSequence = isSequence, extraFiles = extraFiles)
        
        localFiles <- filelist$files
        newFilenames <- filelist$names
      }
      
      self$initLog()
      self$writeLog("Upload")
      self$writeLog(localFiles)
      
      if (!is.null(newFilenames)) {
        self$writeLog("as")
        self$writeLog(localFiles)
      }
      
      # upload image to server
      self$sshConnection()$syncRemoteFiles(
        localFiles, localDir,
        remoteDir,
        fileNames = newFilenames,
        ...
      )
      
      # remove files after transfer
      if (removeFiles == TRUE)
        unlink(localFiles)
      
      self$writeLog("Done")
      self$exitLog()
    },
    
    # retrieve files
    retrieve = function(remoteFiles, localDir, remoteDir,
                        syncExt = NULL, ...) {
      self$initLog()
      self$writeLog("Retrieve")
      self$writeLog(remoteFiles)
      
      # complete filepath
      filesToGet <- paste(
        remoteDir, remoteFiles, sep = "/"
      )
      
      # get files that match the name but not the extension
      if (!is.null(syncExt)) {
        filesToGet <- paste0(filesToGet, syncExt)
      }
      
      # retrieve data from server
      self$sshConnection()$syncLocalFiles(
        filesToGet, remoteDir, localDir, ...)
      
      self$writeLog("Done")
      self$exitLog()
    }
  )
)