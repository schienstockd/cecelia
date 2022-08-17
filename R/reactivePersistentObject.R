#' Reactive object that can be saved as RDS file
#' 
#' @name ReactivePersistentObject
#' @description Persistent version of reactive object
#'
#' @examples
#' TODO
#' @export
ReactivePersistentObject <- R6::R6Class(
  "ReactivePersistentObject",
  inherit = ReactiveObject,
  
  private = list(
    stateFile = NULL,
    uID = NULL,
    initTransaction = FALSE,
    handleLockReleased = FALSE,
    
    # metadata
    cciaName = NULL,
    cciaType = NULL,
    cciaClass = NULL,
    cciaMeta = list(),
    cciaAttr = list(),
    cciaLogfile = NULL,
    
    #' @description
    #' return lockfile for naive transactions
    lockFile = function() {
      paste0(private$getStateFile(), ".lock")
    },
    
    #' @description
    #' start naive transaction
    startTransaction = function() {
      retVal <- TRUE
      
      if (self$isTransaction() == TRUE) {
        if (file.exists(private$lockFile())) {
          retVal <- FALSE
          attr(retVal, "exception") <- "Object locked"
        } else {
          # lock file
          file.create(private$lockFile())
        }
      }
      
      retVal
    },
    
    #' @description
    #' finish naive transaction
    finishTransaction = function() {
      if (self$isTransaction() == TRUE) {
        if (self$lockReleased() == FALSE) {
          message(sprintf(">> Releasing lock [%s]", self$getUID()))
          
          unlink(private$lockFile())
          private$setLockReleased(TRUE)
        }
      }
    },
    
    ## setters
    
    #' @description Set state file
    #' @param x character
    #' @param invalidate boolean
    setStateFile = function(x, invalidate = TRUE) {
      private$stateFile <- x
      private$invalidate(invalidate = invalidate)
    },
    
    #' @description Set whether to init transation
    #' @param x boolean
    #' @param invalidate boolean
    setInitTransaction = function(x, invalidate = FALSE) {
      private$initTransaction <- x
      # private$invalidate(invalidate = invalidate)
    },
    
    #' @description Set whether lock is released
    #' @param x boolean
    #' @param invalidate boolean
    setLockReleased = function(x, invalidate = FALSE) {
      private$handleLockReleased <- x
      # private$invalidate(invalidate = invalidate)
    },
    
    ## getters
    
    #' @description Get state file
    getStateFile = function() {
      private$stateFile
    },
    
    #' @description Initialise parameters with list
    #' @param initParams list
    initParamsWithList = function(initParams) {
      for (curParam in names(initParams)) {
        curFun <- sprintf("setCcia%s", curParam)
        
        if (curFun %in% names(self)){
          self[[curFun]](initParams[[curParam]])
        }
      }
    },
    
    #' @description Function mapping name for parameters
    #' @param moduleFun character
    moduleFunParamsName = function(moduleFun) {
      sprintf("funParams.%s", moduleFun)
    }
  ),
  
  public = list(
    #' @description Init object
    #' @param stateFile File path to state file
    #' @param uID Unique ID of object
    #' @param initParams list to initialise parameters
    #' @param initReactivity boolean to init reactivity
    #' @param initTransaction boolean to init transaction
    #' @param retrieveState boolean to retrieve state from file
    initialize = function(stateFile, uID = NULL, initParams = NULL,
                          initReactivity = TRUE, initTransaction = FALSE,
                          retrieveState = TRUE) {
      super$initialize(initReactivity = initReactivity)
      
      # init stateFile
      private$setStateFile(stateFile)
      
      # init uID
      if (!is.null(uID)) {
        self$setUID(uID)
      }
      
      # init params
      if (!is.null(initParams)) {
        private$initParamsWithList(initParams)
      }
      
      # retrieve state
      if (retrieveState == TRUE)
        self$retrieveState(initTransaction = initTransaction)
    },
    
    #' @description on finalise
    finalize = function() {
      # remove lock
      private$finishTransaction()
    },
    
    ## state management
    #' @description Save state
    #' TODO this is the most time consuming bit of the whole framework
    #' You should think about how to store data in a database
    #' rather than individual objects?
    #' Saving without compression does not seem to make a difference?
    #' @param saveData boolean to save data
    #' @param compressRDS boolean to compress RDS file
    saveState = function(saveData = TRUE, compressRDS = TRUE) {
      savedValues <- list()
      
      # add functions
      allFunctions <- names(self)
      
      # get all getters
      getterFunctions <- allFunctions[startsWith(allFunctions, "get")]
      
      # save values
      for (i in getterFunctions) {
        curFunctionName <- str_replace(i, "get", "")
        
        # call getter function
        savedValues[[curFunctionName]] <- self[[i]]()
      }
      
      # create directory
      dir.create(dirname(
        private$getStateFile()), showWarnings = FALSE)

      # save to RDS
      saveRDS(savedValues, private$getStateFile(), compress = compressRDS)
      
      # save class
      cat(self$getCciaClass(), file = file.path(
        self$persistentObjectDirectory(), CCID_CLASS_FILE))
      
      # save data if method is implemented
      if (saveData == TRUE && "saveData" %in% names(self)) {
        self$saveData()
      }
      
      # finish transaction
      private$finishTransaction()
    },
    
    #' @description Retrieve state
    #' @param initTransaction boolean to init transaction
    #' @param invalidate boolean to invalidate object
    retrieveState = function(initTransaction = FALSE, invalidate = TRUE) {
      retState <- FALSE
      
      # set whether objects is transcation or not
      private$setInitTransaction(initTransaction)
      
      if (length(private$getStateFile()) > 0 && file.exists(private$getStateFile())) {
        # check whether the file is locked
        retState <- private$startTransaction()
        
        if (retState == TRUE) {
          # load state file
          stateRDS <- readRDS(private$getStateFile())
          
          # restore values
          for (i in names(stateRDS)) {
            self[[paste0("set", i)]](
              stateRDS[[i]], invalidate = invalidate, reset = TRUE)
          }
        }
      }
      
      retState
    },
    
    #' @description Run task
    #' @param funName character for function name
    #' @param funParams list for functions parameters
    #' @param taskID integer for task ID
    #' @param uploadSubmissionFiles boolean to upload submission file to HPC
    #' @param taskConf list for function configuration
    #' @param runInplace boolean to run function in place
    #' @param ... self$taskConf
    runTask = function(funName, funParams = list(), taskID = NULL,
                       uploadSubmissionFiles = TRUE, taskConf = NULL,
                       runInplace = TRUE, ...) {
      # init task launcher
      taskLauncher <- TaskLauncher$new()
      
      # get task vars
      if (is.null(taskConf))
        taskConf <- self$taskConf(funParams = funParams, ...)
      
      taskLauncher$initTask(
        funName,
        taskConf,
        taskID = taskID,
        inplace = runInplace,
        cciaObj = if (runInplace == TRUE) self else NULL)
      
      # prep run
      taskLauncher$prepRun(
        uploadSubmissionFiles = uploadSubmissionFiles,
        remoteAsLocal = taskConf$env$global$env != "local"
        )
      
      # run task
      taskLauncher$run()
      
      taskLauncher
    },
    
    #' @description Task variables for object
    #' @param funParams list for function params
    #' @param envVars list for running environment params
    #' @param hpcDir character for HPC directory
    #' @param pID character for project ID
    #' @param pName character for project name
    #' @param env character for environment name. One of c("local", "hpc").
    taskConf = function(funParams, envVars = list(), hpcDir = NULL,
                        pID = "000000", pName = "NONE", env = "local") {
      # build variables
      vars <- list(
        fun = funParams,
        env = list(
          global = list(
            uID = self$getUID(),
            pID = pID,
            pName = pName,
            env = env
          ),
          local = list(
            dirs = list(
              task = self$persistentObjectDirectory(),
              zero = self$persistentObjectDirectory(zero = TRUE)
            )
          )
        )
      )
      
      # add environment
      if (length(envVars) > 0) {
        vars$env <- append(
          vars$env, envVars
        )
      }
      
      # add HPC directory
      if (!is.null(hpcDir)) {
        vars$env$hpc$dirs <- list(
          task = paste(hpcDir, self$version(), self$getUID(), sep = "/"),
          zero = paste(hpcDir, 0, self$getUID(), sep = "/")
        )
      }
      
      vars
    },
    
    #' @description Current version
    version = function() {
      as.numeric(str_extract(
        self$persistentObjectDirectory(root = TRUE), "[0-9]+$"
      ))
    },
    
    #' @description Summary of object
    summary = function(fields = NULL) {
      summaryList <- list(
        "uID" = self$getUID(),
        "Name" = self$getCciaName(),
        "Type" = self$getCciaType(),
        "Class" = self$getCciaClass(),
        "Meta" = self$getCciaMeta(),
        "Attr" = self$getCciaAttr()
      )
      
      if (!is.null(fields)) {
        # add uID
        fields <- c("uID", fields)
        
        summaryList <- summaryList[
          names(summaryList) %in% fields]
      }
      
      summaryList
    },
    
    #' @description Path to persistent object directory
    #' @param root boolean to get root directory
    #' @param zero boolean to get zero directory
    #' @param uID character for unique ID
    persistentObjectDirectory = function(root = FALSE, zero = FALSE, uID = NULL) {
      retDir <- dirname(private$getStateFile())
      
      # zero?
      if (zero == TRUE) {
        retDir <- file.path(retDir, "..", "..", "0", self$getUID())
      }
      
      # root?
      if (root == TRUE) {
        retDir <- file.path(retDir, "../")
      } else if (!is.null(uID)) {
        retDir <- file.path(retDir, "../", uID)
      }
      
      normalizePath(retDir)
    },
    
    #' @description Files in object directory
    #' @param filename character for file name
    #' @param ... self$persistentObjectDirectory
    persistentObjectDirectoryFile = function(filename, ...) {
      file.path(self$persistentObjectDirectory(...), filename)
    },
    
    #' @description Remove object directory
    #' @param removeZero boolean to remove zero
    deleteObjectDirectory = function(removeZero = FALSE) {
      # unlink zero?
      if (removeZero == TRUE) {
        unlink(self$persistentObjectDirectory(zero = TRUE), recursive = TRUE)
      }
      
      # unlink version
      unlink(self$persistentObjectDirectory(), recursive = TRUE)
    },
    
    #' @description Add attribute
    #' @param objAttrName character for attribute name
    #' @param objAttrVal character for attribute value
    #' @param invalidate boolean to invalidate object
    addCciaAttr = function(objAttrName, objAttrVal, invalidate = TRUE) {
      # add to list if not already present
      objAttr <- self$getCciaAttr()
      
      if (!(objAttrName %in% names(objAttr))) {
        objAttr[[objAttrName]] <- objAttrVal
      }
      
      # push back
      self$setCciaAttr(objAttr, invalidate = invalidate)
    },
    
    #' @description Edit attribute
    #' @param objAttrName character for attribute name
    #' @param objAttrVal character for attribute value
    #' @param addAttr boolean to add attribute if not exists
    #' @param invalidate boolean to invalidate object
    editCciaAttr = function(objAttrName, objAttrVal,
                            addAttr = TRUE, invalidate = TRUE) {
      # edit attribute if exist
      objAttr <- self$getCciaAttr()
      
      # add attribute if not present      
      if (addAttr || objAttrName %in% names(objAttr)) {
        objAttr[[objAttrName]] <- objAttrVal
      }
      
      # push back
      self$setCciaAttr(objAttr, invalidate = invalidate)
    },
    
    #' @description Delete/Remove attribute
    #' @param objAttrName character for attribute name
    #' @param invalidate boolean to invalidate object
    delCciaAttr = function(objAttrName, invalidate = TRUE) {
      # get objects without the specified name
      objAttr <- self$getCciaAttr()
      
      self$setCciaAttr(
        objAttr[objAttrName != names(objAttr)],
        invalidate = invalidate
      )
    },
    
    #' @description Function parameters
    #' @param moduleFun character for module function
    moduleFunParams = function(moduleFun) {
      objMeta <- self$getCciaMeta()
      
      retVal <- NULL
      
      # set name of module function params
      moduleFunParams <- private$moduleFunParamsName(moduleFun)
      
      if (moduleFunParams %in% names(objMeta)) {
        retVal <- self$getCciaMeta()[[moduleFunParams]]
      }
      
      retVal
    },
    
    #' @description Save parameters
    #' @param moduleFun character for module function
    #' @param x list of params
    #' @param invalidate boolean to invalidate object
    saveModuleFunParams = function(moduleFun, x, invalidate = TRUE) {
      objMeta <- self$getCciaMeta()
      
      # set name of module function params
      moduleFunParams <- private$moduleFunParamsName(moduleFun)
      
      objMeta[[moduleFunParams]] <- x
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    #' @description Object files
    objFiles = function() {
      baseFile <- tools::file_path_sans_ext(basename(
        private$getStateFile()
        ))
      
      # return type and object
      file.path(
        self$persistentObjectDirectory(),
        paste0(baseFile, c(".type", ".rds"))
      )
    },
    
    ## setters
    setCciaName = function(x, invalidate = TRUE, reset = FALSE) {
      private$cciaName <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setCciaType = function(x, invalidate = TRUE, reset = FALSE) {
      private$cciaType <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setCciaClass = function(x, invalidate = TRUE, reset = FALSE) {
      private$cciaClass <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setCciaMeta = function(x, invalidate = TRUE, reset = FALSE) {
      private$cciaMeta <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setCciaAttr = function(x, invalidate = TRUE, reset = FALSE) {
      private$cciaAttr <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setCciaLogfile = function(x, valueName = "default",
                              setDefault = TRUE, invalidate = TRUE,
                              reset = FALSE) {
      private$cciaLogfile <- .setVersionedVar(
        private$cciaLogfile, x,
        setDefault = setDefault, valueName = valueName,
        reset = reset
      )
      private$invalidate(invalidate = invalidate)
    },
    
    setUID = function(x, invalidate = TRUE, reset = FALSE) {
      private$uID <- x
      private$invalidate(invalidate = invalidate)
    },
    
    ## getters
    getCciaName = function() {
      private$cciaName
    },
    
    getCciaType = function() {
      private$cciaType
    },
    
    getCciaClass = function() {
      private$cciaClass
    },
    
    getCciaMeta = function() {
      private$cciaMeta
    },
    
    getCciaAttr = function(attrName = NULL) {
      attrVals <- private$cciaAttr
      
      # get specific attribute
      if (!is.null(attrName) && attrName %in% names(private$cciaAttr)) {
        attrVals <- private$cciaAttr[[attrName]]
      }
      
      attrVals
    },
    
    #' @description Current or requested logfile only
    #' @param valueName character for value name
    logfile = function(valueName = NULL) {
      .getVersionedVar(private$cciaLogfile, valueName = valueName)
    },
    
    #' @description Current all logfile names
    logfileNames = function() {
      retVal <- names(private$cciaLogfile)
      
      # add attributes
      attr(retVal, "default") <- attr(private$cciaLogfile, "default")
      
      retVal
    },
    
    getCciaLogfile = function() {
      private$cciaLogfile
    },
    
    getUID = function() {
      private$uID
    },
    
    isTransaction = function() {
      private$initTransaction
    },
    
    lockReleased = function() {
      private$handleLockReleased
    }
  )
)
