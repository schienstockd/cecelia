#' Task Process; every function that is to be called as process has to
#' inherit this class
#' 
#' @name TaskProcess
#' @description Base task process class for all module functions
#'
#' @examples
#' TODO
#' @export
TaskProcess <- R6::R6Class(
  "TaskProcess",
  inherit = TaskCore,
  
  private = list(
    taskLogFile = NULL,
    handleUseSink = FALSE,
    
    # python
    condaEnv = NULL,
    condaDir = NULL,
    
    # current task object
    handleCciaTaskObject = NULL,
    handleCciaImageCollection = NULL,
    
    ## setters
    setUseSink = function(x) {
      private$handleUseSink <- x
    },
    
    setCciaTaskObject = function(x) {
      private$handleCciaTaskObject <- x
    },
    
    setCciaImageCollection = function(x) {
      private$handleCciaImageCollection <- x
    },
    
    setCondaEnv = function(x) {
      private$condaEnv <- x
    },
    
    setCondaDir = function(x) {
      private$condaDir <- x
    },
    
    ## getters
    getCciaTaskObject = function() {
      private$handleCciaTaskObject
    },
    
    getCciaImageCollection = function() {
      private$handleCciaImageCollection
    },
    
    getCondaEnv = function() {
      private$condaEnv
    },
    
    getCondaDir = function() {
      private$condaDir
    }
  ),
  
  public = list(
    #' @description Init
    initialize = function() {
    },
    
    #' @description Prepare run
    #' @param confFile character for path to config file
    prepRun = function(confFile) {
      # read parameters
      conf <- jsonlite::fromJSON(readLines(confFile))
      
      # remove config file
      unlink(confFile)
      
      # aggregate inputs with multiple options into lists
      # to make it easier to access
      # go through functions and find '_%d$'
      inputLists <- names(conf$fun)[!is.na(
        stringr::str_match(names(conf$fun), ".*_[:alnum:]+$")
        )]
      
      # get input names
      inputNames <- unique(stringr::str_extract(inputLists, ".*(?=_)"))
      names(inputNames) <- inputNames
      
      # convert to named list
      inputValueLists <- lapply(inputNames, function(x) {
        y <- inputLists[!is.na(stringr::str_match(inputLists, sprintf("%s_[:alnum:]+", x)))]
        
        # get values
        inputValues <- lapply(conf$fun[y], as.character)
        
        # set names
        names(inputValues) <- stringr::str_extract(y, "(?<=_)[:alnum:]+$")
        
        inputValues
      })
      
      # add value name if not given
      if (!"valueName" %in% names(conf$fun)) {
        conf$fun$valueName <- "default"
      }
      
      # remove names from function list and replace
      # with new named lists
      conf$fun[names(conf$fun) %in% inputLists] <- NULL
      conf$fun <- append(conf$fun, inputValueLists)
      
      # set function config
      private$setTaskConf(conf)
      
      # init python
      # if ("python" %in% names(self$utilsParams())) {
      # if (!is.null(cciaConf()$python$conda$source$env)) {
      if (!purrr::is_empty(cciaConf()$python$conda$source$env)) {
        self$initPy(
          # self$utilsParams()$python$condaEnv,
          # self$utilsParams()$python$condaDir
          # use local config
          cciaConf()$python$conda$source$env
        )
      }
      
      # init log
      self$setTaskLogFile(
        file.path(
          self$envParams()$dirs$task,
          cciaConf()$dirs$tasks$log,
          paste(self$funName(), self$globalParams()$taskID, "log", sep = ".")
        )
      )
    },
    
    #' @description Run task
    run = function() {
      stop("IMPLEMENT 'run'")
    },
    
    #' @description Function name
    funName = function() {
      stop("IMPLEMENT 'funName'")
    },
    
    #' @description Init ssh connection
    sshConnection = function() {
      sshConnection <- NULL
      
      if ("ssh" %in% names(self$utilsParams())) {
        sshConnection <- SshUtils$new(
          username = self$utilsParams()$ssh$username,
          address = self$utilsParams()$ssh$address,
          keyfile = self$utilsParams()$ssh$keyfile
        )
      }
      
      sshConnection
    },
    
    #' @description Get ccia object
    #' @param uID character for unique ID
    #' @param ... passed to initCciaObject
    initCciaObject = function(uID, ...) {
      initCciaObject(
        file.path(
          self$cciaTaskObject()$persistentObjectDirectory(root = TRUE), uID
        ), initReactivity = FALSE, ...
      )
    },
    
    #' @description Get task object
    #' @param forceReload boolean to force reload object from disk
    cciaTaskObject = function(forceReload = FALSE) {
      if (is.null(private$handleCciaTaskObject) || forceReload == TRUE) {
        # init object
        cciaObj <- initCciaObject(
          self$envParams()$dirs$task, initReactivity = FALSE
        )
        
        # set object
        private$setCciaTaskObject(cciaObj)
      }
      
      private$getCciaTaskObject()
    },
    
    #' @description Get image collection
    cciaImageCollection = function() {
      if (is.null(private$handleCciaImageCollection)) {
        # init object
        cciaObj <- initCciaObject(
          file.path(
            self$cciaTaskObject()$persistentObjectDirectory(root = TRUE),
            CCID_IMAGE_COLLECTION
          ),
          initReactivity = FALSE
        )
        
        # set object
        private$setCciaImageCollection(cciaObj)
      }
      
      private$getCciaImageCollection()
    },
    
    #' @description Get napari viewer
    napariViewer = function() {
      # init napari
      viewer <- NapariUtils$new(useConnectionFile = TRUE)
      
      viewer$initNapari()
      
      viewer
    },
    
    #' @description Init log
    #' @param useSink boolean to use sink - this does not work
    initLog = function(useSink = FALSE) {
      if (useSink == TRUE) {
        private$setUseSink(TRUE)
        
        sink(self$getTaskLogFile())
      }
      
      write("*** START PROCESS ***", file = self$getTaskLogFile())
    },
    
    #' @description Reinit log
    reinitLog = function() {
      write(">> REINIT LOG", file = self$getTaskLogFile(), append = TRUE)
    },
    
    #' @description Remove log
    exitLog = function() {
      self$writeLog("*** END PROCESS ***")
      
      if (self$useSink() == TRUE) {
        sink()
      }
    },
    
    #' @description Write to log file
    #' @param msg character for logfile
    writeLog = function(msg, printMsg = TRUE) {
      if (printMsg == TRUE)
        print(msg)
      write(msg, file = self$getTaskLogFile(), append = TRUE)
    },
    
    #' @description Check that the function parameter is not null
    #' @param param character for parameter
    funParamNotNull = function(param) {
      notNull <- FALSE
      
      if (param %in% names(self$funParams())) {
        if (!is.null(self$funParams()[[param]])) {
          notNull <- TRUE
        }
      }
      
      notNull
    },
    
    #' @description Get parameter
    #' @param param character for parameter
    funParam = function(param) {
      retVal <- NULL
      
      # check if param exists
      if (self$funParamNotNull(param) == TRUE) {
        retVal <- self$funParams()[[param]]
      }
      
      retVal
    },
    
    #' @description Add function parameter to list
    #' @param paramList list of list of generic
    #' @param param character for parameter
    addFunParamToList = function(paramList, param) {
      # get value
      paramVal <- self$funParam(param)
      
      if (!is.null(paramVal)) {
        paramList[[param]] <- paramVal
      }
      
      paramList
    },
    
    #' @description Add fun params to list
    #' @param paramList list of list of generic
    #' @param params list of list of generic
    addFunParamsToList = function(paramList, params) {
      # add params to list if they exist
      for (x in params) {
        paramList <- self$addFunParamToList(paramList, x)
      }
      
      paramList
    },
    
    #' @description Get class directory
    classDir = function() {
      # get top directories from name
      funPath <- strsplit(self$funName(), paste0("\\", CCID_CLASS_SEP))[[1]]
      funPath <- paste(
        funPath[1:length(funPath) - 1],
        collapse = .Platform$file.sep)
      
      file.path(
        cciaConf()$tasks$sources, funPath
      )
    },
    
    #' @description Init python environment
    #' @param condaEnv character for conda environment
    initPy = function(condaEnv = NULL) {
      private$setCondaEnv(condaEnv)
      # private$setCondaDir(condaDir)
    },
    
    #' @description Run python script
    #' @param scriptFile character for script file
    #' @param paramsList list of list of generic for parameters
    pyScript = function(scriptFile, paramsList = list()) {
      cmd <- c()
      
      # This should be done already in parent process
      # # init conda if necessary
      # if (!purrr::is_empty(private$getCondaEnv())) {
      #   reticulate::use_condaenv(private$getCondaEnv(), required = TRUE)
      # }

      # set working working directory
      os <- reticulate::import("os")
      os$chdir(system.file(".", package = "cecelia"))
      
      # add logfile
      paramsList[["ccia"]] <- list(
        logfile = self$getTaskLogFile(),
        append_log = TRUE
      )
      
      # add value name
      if ("valueName" %in% names(self$funParams())) {
        paramsList[["ccia"]][["value_name"]] <- self$funParams()$valueName
      } else {
        paramsList[["ccia"]][["value_name"]] <- "default"
      }
      
      # add ccia path
      if ("cciaPath" %in% names(self$funParams())) {
        paramsList[["ccia"]][["ccia_path"]] <- self$funParams()$cciaPath
      } else {
        paramsList[["ccia"]][["ccia_path"]] <- cciaPath()
      }
      
      # add channel names
      if ("channelNames" %in% names(self$funParams())) {
        paramsList[["ccia"]][["channel_names"]] <- self$funParams()$channelNames
      } else if ("imChannelNames" %in% names(self$cciaTaskObject())) {
        # paramsList[["ccia"]][["channel_names"]] <- self$cciaTaskObject()$imChannelNames()
        paramsList[["ccia"]][["channel_names"]] <- unname(
          .flowCorrectChannelNames(self$cciaTaskObject()$imChannelNames())
          )
      }
      
      # save as combined JSON
      exportJSON <- jsonlite::toJSON(paramsList)
      
      # generate params file
      paramsFile <- file.path(
        self$envParams()$dirs$task,
        sprintf("pyParams_%s.json", genUID(6))
      )
      
      # save in task directory
      write(exportJSON, paramsFile)
      
      # add python call
      cmd <- c(
        cmd,
        sprintf(
          "python %s %s",
          file.path(self$classDir(), "py",
                    paste(scriptFile, "py", sep = ".")
                    ),
          sprintf("--params \"%s\"", paramsFile)
        )
      )
      
      # collapse
      cmd <- paste(cmd, collapse = ";")
      
      self$writeLog(">> EXEC PY")
      # self$writeLog(paramsList)
      self$writeLog(cmd)
      
      # call python
      handleSystem(.execSystem(cmd))
    },
    
    ## setters
    setTaskLogFile = function(x) {
      private$taskLogFile <- x
    },
    
    ## getters
    getTaskLogFile = function() {
      private$taskLogFile
    },
    
    useSink = function() {
      private$handleUseSink
    }
  )
)
