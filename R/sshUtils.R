#' Ssh utils
#' 
#' @name SshUtils
#' @description
#' Very naive system based ssh utils. This should be replaced with:
#' https://github.com/ropensci/ssh
#'
#' @examples
#' TODO
#' @export
SshUtils <- R6::R6Class(
  "SshUtils",
  
  ## public
  public = list(
    #' @description Init
    #' @param projectManager ProjectManager to init connection
    #' @param username character for username
    #' @param address character for address
    #' @param keyfile character for keyfile path
    initialize = function(projectManager = NULL, username = NULL, address = NULL, keyfile = NULL) {
      # set credentials with projectManager
      if (!is.null(projectManager)) {
        private$sshUsername <- projectManager()$getProjectHPCusername()
        private$sshAddress <- projectManager()$getProjectHPCaddress()
        private$sshKeyfile <- projectManager()$getProjectHPCsshKeyfile()
      }
      
      # set credentials with values
      if (!is.null(username) && !is.null(address)) {
        private$sshUsername <- username
        private$sshAddress <- address
        private$sshKeyfile <- keyfile
      }
    },
    
    #' @description Test connection
    testConnection = function(){
      retVal <- TRUE
      
      # run
      tryCatch(
        expr = {
          handleSystem(self$sshExecute("exit"))
        },
        error = function(e){ 
          retVal <<- FALSE
        },
        warning = function(w){
          retVal <<- FALSE
        }
      )
      
      retVal
    },
    
    #' @description execute command
    #' @param expr character to execute ssh call
    #' @param outputFile character to output file
    #' @param ... passed to private$execCmd
    sshExecute = function(expr, outputFile = NULL, ...) {
      retVal <- NULL
      
      # prepare output statement
      outputExpr <- ""
      
      if (!is.null(outputFile)) {
        outputExpr <- sprintf(" > %s", outputFile)
      }
      
      # TODO that should probably be more elegant to handle errors
      try({
        # check that keyfile exists
        if (file.exists(private$sshKeyfile)) {
          # build ssh string to execute slurm
          # https://stackoverflow.com/questions/30469813/running-a-program-through-ssh-fails-with-error-opening-terminal-unknown
          # https://stackoverflow.com/questions/7114990/pseudo-terminal-will-not-be-allocated-because-stdin-is-not-a-terminal
          # https://askubuntu.com/a/1091554
          sshCmd <- sprintf(
            "export TERM=linux && ssh -tt -o ConnectTimeout=%d -o StrictHostKeyChecking=no -i \"%s\" %s@%s \"%s\" %s",
            cciaConf()$hpc$connectTimeout,
            private$sshKeyfile,
            private$sshUsername,
            private$sshAddress,
            expr,
            outputExpr
          )
          
          retVal <- private$execCmd(sshCmd, ...)
        }
      })
      
      return(retVal)
    },
    
    #' @description Copy files to HPC
    #' @param ... passed to private$syncFiles
    syncRemoteFiles = function(...) {
      private$syncFiles(toHPC = TRUE, ...)
    },
    
    #' @description Copy files to local
    #' @param ... passed to private$syncFiles
    syncLocalFiles = function(...) {
      private$syncFiles(toHPC = FALSE, ...)
    }
  ),
  
  ## private
  private = list(
    sshUsername = NULL,
    sshAddress = NULL,
    sshKeyfile = NULL,
    
    #' @description Exec command on console
    #' @param expr character to execute call
    #' @param wrapWithSystem boolean to wrap call with 'system'
    #' @param stringOnly boolean to return character only
    #' @param intern boolean to execute task intern
    #' @param ... passe to .execSystem
    execCmd = function(
      expr, wrapWithSystem = FALSE, stringOnly = FALSE, intern = TRUE, ...) {
      # execute now or wrap in system for later?
      if (stringOnly == TRUE) {
        return(sprintf("%s", expr))
      } else if (wrapWithSystem == TRUE) {
        return(sprintf("system('%s', intern = %s)", expr, intern))
      } else {
        .execSystem(expr, intern = intern, ...)
      }
    },
    
    #' @description Sync files
    #' @param filesToCopy list of character of files to copy
    #' @param fromDir character to define "from" directory
    #' @param toDir character to define "to" directory
    #' @param createDir boolean to create dir
    #' @param toHPC boolean to sync to HPC
    #' @param fileNames list of character of file names
    #' @param silent boolean to execute command silent
    #' @param useCompression boolean to compress files
    #' @param useArchive boolean to archive files
    #' @param ... passed to private$execCmd
    syncFiles = function(
      filesToCopy, fromDir, toDir,
      createDir = TRUE, toHPC = FALSE,
      fileNames = NULL, silent = FALSE,
      useCompression = FALSE, useArchive = FALSE,
      removeLocalFiles = FALSE, ...) {
      # set files to copy if NULL
      if (is.null(filesToCopy)) {
        filesToCopy <- list.files(
          fromDir,
          recursive = TRUE, full.names = TRUE
        )
      }
      
      # go through files and get the base directories
      toDirs <- dirname(stringr::str_match(
        filesToCopy,
        sprintf(
          "(?<=%s).*", fromDir
        )
      ))
      
      # if syncing files between task v HPC directory
      if (length(toDirs) > 0){
        toDirs <- paste(toDir, toDirs, sep = "/")
      } else {
        toDirs <- rep(toDir, length(filesToCopy))
      }
      
      # create processing directories on HPC
      if (createDir == TRUE) {
        mkdirCmd <- sprintf(
          "mkdir -p %s",
          paste(toDirs, collapse = " ")
        )
        
        if (toHPC == TRUE) {
          handleSystem(self$sshExecute(mkdirCmd), silent = silent)
        } else {
          private$execCmd(mkdirCmd)
        }
      }
      
      # add filenames
      if (!is.null(fileNames)) {
        toDirs <- paste(toDirs, fileNames, sep = "/")
      }
      
      # to or from HPC?
      sftpCmd <- if (toHPC == TRUE) "put" else "get"
      
      # prepare sftp call
      sftpCall <- "sftp"
      
      if (useCompression == TRUE) {
        sftpCall <- paste(sftpCall, "-C")
      }
      
      # package into archive?
      # TODO should I do this on the fly for retrieving
      # files from the HPC?
      # https://unix.stackexchange.com/a/10028
      if (useArchive == TRUE) {
        # remove wildcard for archive names
        archiveNames <- filesToCopy
        archiveNamesWildcard <- !is.na(stringr::str_match(filesToCopy, "\\*"))
        archiveNames[archiveNamesWildcard] <- dirname(archiveNames[archiveNamesWildcard])
        
        # get files to archive
        # TODO shorten
        archiveFiles <- basename(filesToCopy)
        archiveFiles[archiveNamesWildcard] <- basename(dirname(filesToCopy[archiveNamesWildcard]))
        archiveFiles[archiveNamesWildcard] <- paste(
          archiveFiles[archiveNamesWildcard],
          basename(filesToCopy[archiveNamesWildcard]),
          sep = "/"
        )
        
        # get basenames
        archiveDirs <- dirname(archiveNames)
        archiveNames <- basename(paste0(archiveNames, ".tar"))
        
        if (toHPC == FALSE) {
          message(">> Archive files")
          
          # prepare tar files
          sshCmd <- mapply(
            function(k, x, i) {
              if (all(is.na(stringr::str_match(i, "\\*")))) {
                sprintf("cd \"%s\"; tar -cf \"%s\" \"%s\"", k, x, i)
              } else {
                sprintf("cd \"%s\"; tar -cf \"%s\" %s", k, x, i)
              }
            },
            archiveDirs,
            archiveNames,
            archiveFiles,
            USE.NAMES = FALSE,
            SIMPLIFY = TRUE
          )
          
          self$sshExecute(paste(
            sprintf("cd \"%s\"", fromDir),
            paste(sshCmd, collapse = ";"),
            sep = ";"
            ))
        } else {
          # TODO
        }
        
        # add files to copy
        filesToCopy <- paste(archiveDirs, archiveNames, sep = "/")
        toDirs[archiveNamesWildcard] <- dirname(toDirs[archiveNamesWildcard])
      }
      
      # allow wildcard expansion
      copyCmd <- mapply(
        function(x, i) {
          if (all(is.na(stringr::str_match(x, "\\*")))) {
            sprintf("%s \"%s\" \"%s\"", sftpCmd, x, i)
          } else {
            sprintf("%s %s \"%s\"", sftpCmd, x, i)
          }
        },
        filesToCopy,
        toDirs,
        USE.NAMES = FALSE,
        SIMPLIFY = TRUE
      )
      
      # copy files
      sshCmd <- sprintf(
        "%s -o StrictHostKeyChecking=no -r -i \"%s\" %s@%s << EOF\n%s\nEOF",
        sftpCall,
        private$sshKeyfile,
        private$sshUsername,
        private$sshAddress,
        paste(
          copyCmd,
          collapse = "\n"
        )
      )
      
      message(">> Transfer files")
      
      # execute copy
      copyResult <- private$execCmd(sshCmd, ...)
      
      # unpack archive
      if (useArchive == TRUE) {
        if (toHPC == FALSE) {
          message(">> Unpack files")
          
          # remove archive files from HPC
          sshCmd <- sprintf("rm \"%s\"", filesToCopy)
          self$sshExecute(paste(sshCmd, collapse = ";"))
          
          # define files to unpack
          filesToUnpack <- basename(filesToCopy)
          unpackCmd <- sprintf(
            "cd \"%s\"; tar xf \"%2$s\" && rm \"%2$s\"",
            # https://serverfault.com/a/270825
            # "cd \"%s\"; pigz -dc \"%2$s\" | tar xf - && rm \"%2$s\"",
            toDirs,
            filesToUnpack)
          
          # unpack
          private$execCmd(paste(unpackCmd, collapse = ";"), ...)
        } else {
          # TODO
        }
      }
    }
  )
)
