# constants
CCID_IMAGE_COLLECTION <- "IIIII0"
CCID_ANALYSIS_COLLECTION <- "AAAAA0"
CCID_STATE_FILE <- "ccid.rds"
CCID_CLASS_FILE <- "ccid.type"
CCID_CLASS_SEP <- "."

# File conventions
fileIMAGE_CONVERTED <- "ccidImage"
fileIMAGE_TO_IMPORT <- "ImageToImport"

#' @description Init ccia object
#' @param cciaObjDir character of object directory
#' @param initReactivity boolean to init reactivity
#' @param initTransaction boolean to init transaction
#' @param waitForRelease boolean to wait for release
#' @param retrieveState boolean to retrieve state
#' @examples
#' TODO
#' @export
initCciaObject <- function(cciaObjDir = NULL, pID = NULL, uID = NULL,
                           versionID = NULL, initReactivity = TRUE,
                           initTransaction = FALSE, waitForRelease = FALSE, 
                           retrieveState = TRUE) {
  # compile object directory if not given
  if (is.null(cciaObjDir)) {
    cciaObjDir <- file.path(
      cciaConf()$dirs$projects, pID, "ANALYSIS", versionID, uID
    )
  }
  
  typePath <- file.path(cciaObjDir, CCID_CLASS_FILE)
  statePath <- file.path(cciaObjDir, CCID_STATE_FILE)
  
  # TODO does that make a difference to init?
  # if (file.exists(typePath)) {
  # determine which classes to initialise
  curClass <- readLines(typePath, warn = FALSE)
  
  # create init object expression
  initObjectStr <- sprintf(
    "cciaObj <- %s$new(\"%s\", initReactivity = %s, initTransaction = %s, retrieveState = %s)",
    curClass, statePath, initReactivity, initTransaction, retrieveState
  )
  
  # prepare init
  curInit <- paste(
    initObjectStr,
    # was the object initialised?
    sprintf("if (%s == FALSE && is.null(cciaObj$getUID())) return(NULL)", waitForRelease),
    # wait for release?
    sprintf("if (%s == TRUE && is.null(cciaObj$getUID())) {", waitForRelease),
      "message('>> Waiting for lock release')",
      "while(is.null(cciaObj$getUID())) {",
        "Sys.sleep(1/10)",
        initObjectStr,
    "}}",
    sep = "\n"
  )
  
  # init reactivity
  if (initReactivity == TRUE) {
    curInit <- paste(curInit,"cciaObj$reactive()",sep = "\n")
  } else {
    # return object
    curInit <- paste(curInit, "cciaObj", sep = "\n")
  }
  
  # message(sprintf(">> Init [%s]", cciaObjDir))
  
  # create class
  eval(parse(text = curInit))
  # }
}

#' @description Stats type of measure
#' TODO improve
#' @param x character of measure
#' @examples
#' TODO
.cciaStatsType <- function(x) {
  # is there a "#" in the name?
  if (grepl("#", x) == TRUE) {
    x <- stringr::str_split(x, "#")[[1]][[1]]
  }
  
  # return type
  if (x %in% names(cciaConf()$parameters$labelStats)) {
    return(cciaConf()$parameters$labelStats[[x]])
  } else {
    return(
      cciaConf()$parameters$labelStats[startsWith(
        x, names(cciaConf()$parameters$labelStats))][[1]]
      )
  }
  
}

#' @description Name for stats
#' Also converts 'XYZ.TRUE' to 'XYZ'
#' @param x character of measure
#' @examples
#' TODO
.cciaStatsName <- function(x) {
  # is there a "#" in the name?
  if (grepl("#", x) == TRUE) {
    x <- stringr::str_split(x, "#")[[1]][[1]]
  }
  
  # return type
  if (x %in% names(cciaConf()$parameters$labelStats)) {
    return(x)
  } else {
    return(
      names(cciaConf()$parameters$labelStats[startsWith(
        x, names(cciaConf()$parameters$labelStats))]
      ))
  }
  
}

#' @description Check whether stats type is logical/categorical
#' @param x character of measure
#' @examples
#' TODO
.cciaStatsTypeIsCategorical <- function(x) {
  .cciaStatsType(x) %in% c("categorical", "logical")
}

#' @description Encrypt character
#' @param x character of measure
#' @param asInteger boolean to convert to integer
#' @examples
#' TODO
#' @export
cciaEncrypt <- function(x, asInteger = TRUE) {
  # very basic encryption for the password
  # TODO could you store that somehow in the keyring .. ?
  # https://stackoverflow.com/a/32359993/13766165
  x <- PKI.encrypt(charToRaw(x), CCIA_SESSION_KEY)
  
  # convert to integer?
  if (asInteger == TRUE){
    x <- as.integer(x)
  }
  
  x
}

#' @description Decrypt character
#' @param x character of measure
#' @param fromInteger boolean to convert from integer
#' @examples
#' TODO
#' @export
cciaDecrypt <- function(x, fromInteger = TRUE) {
  if (fromInteger == TRUE) {
    x <- as.raw(x)
  }
  
  x <- rawToChar(PKI.decrypt(x, CCIA_SESSION_KEY))
  
  x
}

#' @description Normalise DT per batch group
#' @param popDT data.frame of populations
#' @param colsToNormalise list of character of columns to normalise
#' @param normPercentile numeric to normalise columns
#' @param batchGroup character to identify batch groups
#' @examples
#' TODO
.normaliseDT <- function(popDT, colsToNormalise, normPercentile = 0.998, batchGroup = "uID") {
  # make sure the columns are in the DT
  colsToNormalise <- colnames(popDT)[colnames(popDT) %in% colsToNormalise]
  
  # TODO can this be shortened?
  # go through batch groups
  for (batch in unique(popDT[[batchGroup]])) {
    # go through columns to normalise
    for (x in colsToNormalise) {
      y <- popDT[get(batchGroup) == batch, ][[x]]
      
      # get percentile
      maxPercentile <- quantile(y, normPercentile, na.rm = TRUE)
      minPercentile <- quantile(y, 1 - normPercentile, na.rm = TRUE)
      
      # adjust column
      normY = (y - minPercentile) / (maxPercentile - minPercentile)
      normY[normY < 0] <- 0
      normY[normY > 1] <- 1
      
      popDT[get(batchGroup) == batch, (x) := normY]
    }
  }
  
  popDT
}

#' @description Set versioned variable in ccia itemValue
#' @param valueList list of values
#' @param itemValue generic of item value
#' @param setDefault boolean to set as default
#' @param valueName character of value name
#' @param reset boolean to reset value
#' @examples
#' TODO
.setVersionedVar <- function(valueList, itemValue,
                            setDefault = TRUE, valueName = NULL,
                            reset = FALSE) {
  retVal <- NULL
  
  # get default from value
  if (is.null(valueName)) {
    valueName <- "default"
  }
  
  # reset?
  if (reset == TRUE) {
    retVal <- itemValue
  } else {
    if (is.null(valueList) || !is.list(valueList)) {
      # if values were stored as single values before
      retVal <- list(default = valueList)
    } else {
      retVal <- valueList
    }
    
    retVal[[valueName]] <- itemValue
      
    # set as default
    if (setDefault == TRUE) {
      attr(retVal, "default") <- valueName
    }
    
    # reset to default if value is NULL
    if (is.null(itemValue)) {
      attr(retVal, "default") <- "default"
    }
  }
  
  retVal
}

#' @description Set versioned variable list in ccia Object
#' @param attrList list of attributes
#' @param itemValue generic of item value
#' @param setDefault boolean to set as default
#' @param valueName character of value name
#' @param reset boolean to reset value
#' @examples
#' TODO
.setVersionedVarInList <- function(attrList, itemName, itemValue,
                                 setDefault = TRUE, valueName = NULL,
                                 reset = FALSE) {
  # set value name to default if not given
  if (is.null(valueName)) {
    valueName <- "default"
  }
  
  # set list
  if (reset == TRUE) {
    attrList[[itemName]] <- itemValue
  } else {
    # add list item
    if (!itemName %in% names(attrList)) {
      attrList[[itemName]] <- list()
      attrList[[itemName]][[valueName]] <- itemValue
    } else {
      if (!is.list(attrList[[itemName]])) {
        # if values were stored as single values before
        attrList[[itemName]] <- list(default = attrList[[itemName]])
      }
      
      attrList[[itemName]][[valueName]] <- itemValue
    }
    
    # set as default
    if (setDefault == TRUE) {
      attr(attrList[[itemName]], "default") <- valueName
    }
    
    # reset to default if value is NULL
    if (is.null(itemValue)) {
      attr(attrList[[itemName]], "default") <- "default"
    }
  }
  
  attrList
}

#' @description Get value from versioned variable in ccia Object
#' @param valueList list of values
#' @param valueName character of value name
#' @examples
#' TODO
.getVersionedVar <- function(valueList, valueName = NULL) {
  retVal <- NULL
  
  if (!is.null(valueList)) {
    # return default
    if (is.null(valueName)) {
      valueName <- attr(valueList, "default")
      
      # if there is no default
      if (is.null(valueName)) {
        valueName <- "default"
        
        retVal <- valueList
      }
    }
    
    if (!is.null(valueList) && is.null(retVal)) {
      if (valueName %in% names(valueList)) {
        retVal <- valueList[[valueName]]
        
        # set name for value
        if (!is.null(retVal))
          attr(retVal, "valueName") <- valueName
      }
    }
  }
  
  retVal
}

#' @description Get value from versioned variable list in ccia Object
#' @param valueList list of list of values
#' @param itemName character of item name
#' @param valueName character of value name
#' @examples
#' TODO
.getVersionedVarInList <- function(attrList, itemName,
                                 valueName = NULL) {
  retVal <- NULL
  
  if (itemName %in% names(attrList)) {
    retVal <- .getVersionedVar(
      attrList[[itemName]], valueName = valueName
    )
  }
  
  retVal
}

#' @description Trim module function name
#' @param x character of module function
#' @examples
#' TODO
.trimModuleFunName <- function(x) {
  xTrimmed <- x
  
  # check if there is something to trim
  if (!any(is.na(stringr::str_match(xTrimmed, paste0("\\", CCID_CLASS_SEP))))) {
    xTrimmed <- stringr::str_split(xTrimmed, paste0("\\", CCID_CLASS_SEP))[[1]]
    
    # get last element
    xTrimmed <- xTrimmed[[length(xTrimmed)]]
  }
  
  xTrimmed
}

#' @description Exec system command
#' @param cmd character of system command
#' @param intern boolean of using intern
#' @param ... passed to system
#' @examples
#' TODO
.execSystem <- function(cmd, intern = TRUE, ...) {
  suppressWarnings(
    system(cmd, intern = intern, ...)
  )
}

#' @description Create task vars
#' @param uID character of unique ID
#' @param projectManager ceceliaApp::ProjectManager
#' @param taskEnv character of task environment
#' @param taskHPCnumNodes integer of number of HPC nodes
#' @param taskHPCnumTasks integer of number of HPC tasks
#' @param taskHPCnumCPUperTask integer of number of HPC CPUs per task
#' @param taskHPCnumGPUperTask integer of number of HPC GPUs per task
#' @param taskHPCmemory integer of HPC memory in 'GB'
#' @param taskHPCwalltime character of walltime 'dd-hh:mm:ss'
#' @param hpcUseGPU boolean to use GPUs
#' @examples
#' TODO
#' @export
createTaskVars <- function(uID, projectManager, taskEnv,
                           taskHPCnumNodes = 1, taskHPCnumTasks = 1,
                           taskHPCnumCPUperTask = 1, taskHPCnumGPUperTask = 1,
                           taskHPCmemory = 50, taskHPCwalltime = "00-01:00:00",
                           hpcUseGPU = FALSE) {
  # set HPC settings
  if (hpcUseGPU == TRUE) {
    hpcProjectPartitions = projectManager$getProjectHPCpartitionsGPU()
    hpcProjectID = projectManager$getProjectHPCprojectGPU()
  } else {
    hpcProjectPartitions = projectManager$getProjectHPCpartitionsCPU()
    hpcProjectID = projectManager$getProjectHPCprojectCPU()
  }
  
  hpcProjectID <- paste0(cciaConf()$hpc$dirs$projectPrefix, hpcProjectID)
  
  list(
    env = list(
      global = list(
        uID = uID,
        pID = projectManager$getProjectUID(),
        pName = projectManager$getProjectName(),
        env = taskEnv,
        projectsDir = projectManager$getProjectPath()
      ),
      local = list(
        dirs = list(
          task = projectManager$persistentObjectDirectory(uID),
          zero = projectManager$persistentObjectDirectory(uID, version = 0)
        )
      ),
      hpc = list(
        dirs = list(
          task = projectManager$persistentObjectHPCDirectory(uID),
          zero = projectManager$persistentObjectHPCDirectory(uID, version = 0)
        ),
        conf = list(
          email = projectManager$getProjectHPCemail(),
          emailOnBegin = projectManager$getProjectHPCemailOnBegin(),
          emailOnEnd = projectManager$getProjectHPCemailOnEnd(),
          emailOnFail = projectManager$getProjectHPCemailOnFail(),
          numNodes = taskHPCnumNodes,
          numTasks = taskHPCnumTasks,
          numCPUperTask = taskHPCnumCPUperTask,
          numGPUperTask = taskHPCnumGPUperTask,
          memory = taskHPCmemory,
          walltime = taskHPCwalltime,
          projectPartitions = hpcProjectPartitions,
          projectID = hpcProjectID,
          useGPU = hpcUseGPU
        )
      ),
      utils = list(
        ssh = list(
          username = projectManager$getProjectHPCusername(),
          address = projectManager$getProjectHPCaddress(),
          keyfile = projectManager$getProjectHPCsshKeyfile()
        ),
        smb = list(
          username = projectManager$getProjectLabServerSmbUser(),
          password = projectManager$projectLabServerSmbPwd(),
          remoteDir = projectManager$getProjectLabServerSmbRemoteDir(),
          localDir = projectManager$getProjectLabServerSmbLocalMountDir()
        )
        # use local config files
        # python = list(
        #   condaEnv = cciaConf()$python$conda$source$env,
        #   condaDir = cciaConf()$python$conda$dir
        # )
      )
    )
  )
}

#' @description Prep character for bash command
#' @param x character to use in bash
#' @param quotingStyle character which quating style to use, any of c("single", "double")
#' @examples
#' TODO
.prepForBash <- function(x, quotingStyle = "single") {
  # add extra backslashes
  x <- stringr::str_replace_all(x, fixed("\\"), r"(\\\\)")
  
  # beware of quotes
  if (quotingStyle == "single")
    x <- stringr::str_replace_all(x, fixed("'"), r"(\')")
  else
    x <- stringr::str_replace_all(x, fixed("'"), r"(\")")
  
  x
}

#' @description Read log file
#' @param logFile character of log file path
#' @param previousContent character of previous content
#' @param mergeContent boolean to merge content
#' @examples
#' TODO
#' @export
readLogFile <- function(logFile, previousContent = NULL, mergeContent = TRUE) {
  # check time
  # does not work well, use size
  logSize <- file.size(logFile)
  # logMtime <- file.mtime(logFile)
  
  # get attributes
  previousState <- 0
  prevLineReads <- 0
  
  output <- NULL
  
  # get attributes for merge
  if (mergeContent == TRUE) {
    if (!is.null(attr(previousContent, "state"))) 
      previousState <- attr(previousContent, "state")
    if (!is.null(attr(previousContent, "lineReads"))) 
      prevLineReads <- attr(previousContent, "lineReads")
    
    # reset
    if (!is.null(attr(previousContent, "updated")))
      attr(previousContent, "updated") <- FALSE
    
    output <- previousContent
  } else {
    # reset to whole file
    previousContent <- NULL
  }
  
  if (!is.na(logSize) && logSize > previousState) {
    # logMtime != previousState) {
    
    # read content
    # be sure the file exists when reading
    # sometimes I get
    # Warning: Error in read_lines_: Cannot read file ...: Invalid argument
    # if (file.exists(logFile))
    logContent <- NULL
    try({
      logContent <- read_lines(logFile, skip = prevLineReads)
    })
    
    # remember lines
    curLineReads <- length(logContent)
    
    # collapse
    logContent <- paste(logContent, collapse = "\n")
    
    # set line reads
    lineReads <- prevLineReads + curLineReads
    
    output <- paste(c(
      # there are sometimes new line markers at the end
      trimws(previousContent), logContent
    ), collapse = "\n")
    
    # remember attributes
    attr(output, "state") <- logSize
    attr(output, "lineReads") <- lineReads
    attr(output, "updated") <- TRUE
    attr(output, "updatedContent") <- logContent
  }
  
  output
}

# # rename channel columns
# renameChannelColumns <- function(df, channelNames, channelPattern = "mean_intensity", 
#                                  flowNames = TRUE) {
#   # correct names as 'flow' names
#   if (flowNames == TRUE) {
#     channelNames <- .flowCorrectChannelNames(channelNames)
#   }
#   
#   # rename all channels matching the pattern
#   names(df)[c(!is.na(stringr::str_match(names(df), channelPattern)))] <- channelNames
#   
#   df
# }

#' @description Convert DT with pixels to um
#' @param DT data.table to convert
#' @param pixelRes list of numeric for pixel resolution
#' @examples
#' TODO
#' @export
convertPixelToPhysical <- function(DT, pixelRes) {
  # convert xy and then z
  xyCols <- c("centroid_x", "centroid_y")
  DT[, (xyCols) := lapply(.SD, function(x) {
    x * pixelRes$x
  }), .SDcols = xyCols]
  
  if (!is.na(pixelRes$z) && "centroid_z" %in% names(DT)) {
    DT[, centroid_z := .(centroid_z * pixelRes$z)]
  }
  
  DT
}

#' @description Get first selected image from set
#' @param selectedUIDs list of character for selected unique IDs
#' @param selectedSet reactivePersistentObjectSet
#' @examples
#' TODO
#' @export
firstSelectedImageFromSet <- function(selectedUIDs, selectedSet) {
  cciaObj <- NULL

  # are any of the selected images in the current set?
  if (any(selectedUIDs %in% selectedSet$cciaObjectUIDs())) {
    # get first selected object
    uID <- selectedUIDs[selectedUIDs %in% selectedSet$cciaObjectUIDs()]
    
    if (length(uID) > 1) {
      uID <- uID[[1]]
    }
    
    cciaObj <- selectedSet$cciaObjectByUID(uID)
    cciaObj <- cciaObj[[1]]()
  }
  
  cciaObj
}


#' @description Rename channel columns
#' @param channelList list of character for channels
#' @param channelNames list of character for channels names
#' @param channelPattern character of channel pattern
#' @param flowNames boolean to convert names to 'flow save names'
#' @examples
#' TODO
#' @export
renameChannelColumns <- function(channelList, channelNames, channelPattern = "mean_intensity",
                                 flowNames = TRUE) {
  # correct names as 'flow' names
  if (flowNames == TRUE) {
    channelNames <- .flowCorrectChannelNames(channelNames)
  }

  # rename all channels matching the pattern
  channelList[c(!is.na(stringr::str_match(channelList, channelPattern)))] <- channelNames

  channelList
}

#' @description Normalise DT column
#' @param DT data.frame of population
#' @param col character of column
#' @param normCol character of new column name
#' @param percentile numeric of normalisation percentile
#' @examples
#' TODO
.normDTCol <- function(DT, col, normCol = col, percentile = 0.98) {
  maxSubtract <- quantile(
    DT[[col]], percentile, na.rm = TRUE)
  minSubtract <- quantile(
    DT[[col]], 1 - percentile, na.rm = TRUE)
  
  # adjust column
  DT[, c(normCol) := (
    (get(col) - minSubtract) / (maxSubtract - minSubtract)
  )]
  DT[get(normCol) < 0, c(normCol) := 0]
  DT[get(normCol) > 1, c(normCol) := 1]
}

#' @description Get option from list
#' @param optList list of list of character with options
#' @param optKey character of option key
#' @param defaultVal character of default value if key not found
#' @examples
#' TODO
.optFromList <- function(optList, optKey, defaultVal = NULL) {
  retVal <- defaultVal
  
  # get value from list
  if (optKey %in% names(optList))
    retVal <- optList[[optKey]]
  
  retVal
}

#' @description Get options from list
#' @param optList list of list of character with options
#' @param optKeys list of character of option keys
#' @param ... passed to .optFromList
#' @examples
#' TODO
#' @export
optsFromList <- function(optList, optKeys, ...) {
  # go through keys
  for (i in names(optKeys)) {
    defaultVal <- optKeys[[i]]
    
    # get value
    optKeys[[i]] <- .optFromList(optList, i, ...)
  }
  
  optKeys
}

#' @description Handle system return
#' @param retVal character with attributes of system return
#' @param silent boolean to run command silently
#' @examples
#' TODO
#' @export
handleSystem <- function(retVal, silent = FALSE) {
  # handle return
  if ("status" %in% names(attributes(retVal)) && attr(retVal, "status") > 0) {
    # stop?
    if (silent == FALSE) {
      print(">> SYSTEM ERROR")
      stop()
    } else {
      print(">> SYSTEM SILENT ERROR")
    }
    
    print(retVal)
  }
  
  invisible()
}

#' @description Check whether task was successful
#' @param res character with attributes of result
#' @examples
#' TODO
#' @export
taskResultSuccess <- function(res) {
  !is.list(res) || length(res) != 1 || !inherits(res[[1]], "try-error")
}

#' @description Return files in task directory
#' @param taskDir character of task directory
#' @param valueNames list of character of value names
#' @param isDir boolean whether value names is directory
#' @examples
#' TODO
#' @export
taskDirFiles <- function(taskDir, valueNames, isDir = FALSE) {
  if (length(valueNames) > 0) {
    if (isDir == TRUE) {
      # return wildcard for file type
      paste(
        cciaConf()$dirs$tasks[[taskDir]],
        valueNames,
        paste0("*", cciaConf()$files$ext[[taskDir]]),
        sep = "/")
    } else {
      paste(
        cciaConf()$dirs$tasks[[taskDir]],
        paste0(valueNames,
               cciaConf()$files$ext[[taskDir]]),
        sep = "/")
    }
  }
}

#' @description Get pop types from pops
#' @param pops list of character of populations
#' @param popSplit character of population split
#' @examples
#' TODO
#' @export
popTypesFromPops <- function(pops, popSplit = "\\.") {
  unique(sapply(
    pops, function(x) unlist(stringr::str_split(x, popSplit, n = 2))[[1]]
  ))
}

#' @description Get pop paths from pops
#' @param pops list of character of populations
#' @param popSplit character of population split
#' @examples
#' TODO
#' @export
popPathsFromPops <- function(pops, popSplit = "\\.") {
  unique(sapply(
    pops, function(x) unlist(stringr::str_split(x, popSplit, n = 2))[[2]]
  ))
}

#' @description Get leaves for parent
#' @param pops list of character of populations
#' @param pop character of population
#' @examples
#' TODO
.popsGetParentLeaves <- function(pops, pop = "/") {
  # get all leaves
  if (!.flowPopIsRoot(pop)) {
    # https://stackoverflow.com/a/27721009/13766165
    # allLeaves <- unlist(as.list(stringr::str_match(pops, sprintf("^[%s/.+$", pop))))
    allLeaves <- pops[startsWith(unlist(pops), .flowNormRootPath(pop, addSuffix = TRUE))]
    allLeaves <- allLeaves[allLeaves != pop]
  } else {
    allLeaves <- pops[!is.na(stringr::str_match(pops, "^/.+$"))]
  }
  
  allLeaves[!is.na(allLeaves)]
}

#' @description capitalise first letter
#' @param x character to modify
firstToupper <- function(x) {
  paste0(
    toupper(substr(x, 1, 1)),
    substring(x, 2)
  )
}

#' @description Apply filter to population
# TODO is there a better way to do this?
#' @param popEntry list of character entry from population map
#' @param popDT data.table of population
#' @examples
#' TODO
.popsApplyFilterToPopDT <- function(popEntry, popDT) {
  # get filter measures
  # TODO Do I need to duplicate here because values could be a list?
  if ("filterMeasures" %in% names(popEntry)) {
    i <- 1
    for (x in popEntry$filterMeasures) {
      if (popEntry$filterFuns[[i]] == "gt") {
        popDT <- popDT[
          get(x) > popEntry$filterValues[[i]], ]
      } else if (popEntry$filterFuns[[i]] == "gte") {
        popDT <- popDT[
          get(x) >= popEntry$filterValues[[i]], ]
      } else if (popEntry$filterFuns[[i]] == "lt") {
        popDT <- popDT[
          get(x) < popEntry$filterValues[[i]], ]
      } else if (popEntry$filterFuns[[i]] == "lte") {
        popDT <- popDT[
          get(x) <= popEntry$filterValues[[i]], ]
      } else if (popEntry$filterFuns[[i]] == "eq") {
        popDT <- popDT[
          get(x) %in% popEntry$filterValues[[i]], ]
      } else if (popEntry$filterFuns[[i]] == "neq") {
        popDT <- popDT[
          !get(x) %in% popEntry$filterValues[[i]], ]
      }
      
      i <- i + 1
    }
  } else {
    if (popEntry$filterFun == "gt") {
      popDT <- popDT[
        get(popEntry$filterMeasure) > popEntry$filterValues, ]
    } else if (popEntry$filterFun == "gte") {
      popDT <- popDT[
        get(popEntry$filterMeasure) >= popEntry$filterValues, ]
    } else if (popEntry$filterFun == "lt") {
      popDT <- popDT[
        get(popEntry$filterMeasure) < popEntry$filterValues, ]
    } else if (popEntry$filterFun == "lte") {
      popDT <- popDT[
        get(popEntry$filterMeasure) <= popEntry$filterValues, ]
    } else if (popEntry$filterFun == "eq") {
      popDT <- popDT[
        get(popEntry$filterMeasure) %in% popEntry$filterValues, ]
    } else if (popEntry$filterFun == "neq") {
      popDT <- popDT[
        !get(popEntry$filterMeasure) %in% popEntry$filterValues, ]
    }
  }
  
  popDT
}

#' @description Create average matrix
# TODO is there a better way to do this?
#' @param popDT data.table of population
#' @examples
#' TODO
#' @export
adataMatFromPopDT <- function(popDT) {
  # convert chanels to matrix
  # https://stackoverflow.com/a/43834005/13766165
  adataSummary <- popDT[
    order(clusters), sapply(.SD, function(x) list(mean = mean(x))),
    .SDcols = colnames(popDT)[
      !colnames(popDT) %in% c("uID", "label", "clusters", "pop", paste("UMAP", seq(2), sep = "_"))
    ], by = clusters]
  
  # replace names with readable channel names
  colnames(adataSummary) <- stringr::str_replace(colnames(adataSummary), ".mean", "")
  
  # remove clusters and transform matrix
  anndataMat <- t(as.matrix(
    adataSummary[, !c("clusters")]
  ))
  
  # set names
  colnames(anndataMat) <- adataSummary$clusters
  
  # remove na
  # https://stackoverflow.com/a/6471927
  anndataMat[rowSums(is.na(anndataMat)) != ncol(anndataMat), ]
}

#' @description Prep file list to sync
# TODO is there a better way to do this?
#' @param oldFilename character of old filename
#' @param newFilename character of new filename
#' @examples
#' TODO
#' @export
prepFilelistToSync <- function(oldFilename, newFilename) {
  # get new file name
  fileExt <- file_ext(oldFilename)
  
  filesToCopy <- c(oldFilename)
  newFileNames <- c(sprintf("%s.%s", newFilename, fileExt))
  
  # check for file extensions that save images in multiple files
  if (fileExt %in% cciaConf()$images$splitFileFormats) {
    # get filename
    filename <- tools::file_path_sans_ext(basename(oldFilename))
    addedFiles <- c()
    addedNames <- c()
    
    # Olympus files
    if (fileExt == "oir") {
      # get other files within the same directory that start with that name
      # this does not work for filenames with re patterns, eg/ "basal+NECA"
      # addedFiles <- list.files(dirname(oldFilename), pattern = sprintf("%s_[0-9]+", filename))
      addedFiles <- list.files(dirname(oldFilename))
      
      # get files that match the filename_0000n
      addedFiles <- addedFiles[
        str_sub(addedFiles, start = 1,
                end = stri_locate_last_fixed(addedFiles, "_")[,1] - 1) == filename
      ]
      
      # remove NA
      addedFiles <- addedFiles[!is.na(addedFiles)]
      
      # remove non 5 numeric values
      addedFiles <- addedFiles[!is.na(stringr::str_match(
        str_sub(
          addedFiles,
          start = stri_locate_last_fixed(addedFiles, "_")[,1] + 1),
        "[0-9]{5}"
      ))]
      
      # add names
      # addedNames <- c(paste0(newFilename, stringr::str_replace(addedFiles, filename, "")))
      addedNames <- c(
        paste0(newFilename, str_sub(addedFiles, str_length(filename) + 1))
        )
    }
    
    # copy to list
    filesToCopy <- c(filesToCopy, addedFiles)
    newFileNames <- c(newFileNames, addedNames)
  }
  
  list(
    files = filesToCopy,
    names = newFileNames
  )
}
