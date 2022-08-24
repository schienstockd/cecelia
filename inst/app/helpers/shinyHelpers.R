## Error handling and validation
# https://shiny.rstudio.com/articles/validation.html
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

## Input elements
# generate inputs for a list
shinyInput <- function(FUN.name, id, postfix, startFrom = 1,
                       initVals = NULL, initStyles = NULL, initLabels = NULL,
                       initSelected = NULL, initDisabled = NULL,
                       initOnclick = NULL, initIcons = NULL,
                       initClasses = NULL, initChoices = NULL,
                       ...) {
  inputs <- character(length(postfix))
  
  # get FUN
  FUN <- eval(parse(text = FUN.name))
  
  if (FUN.name == "selectInput") {
    createInput <- function(id, i, elementName) {
      FUN(paste0(id, elementName),
          label = if (!is.null(initLabels)) initLabels[i] else NULL,
          choices = if (!is.null(initChoices)) initChoices[i] else NULL,
          selected = if (!is.null(initSelected)) initSelected[i] else NULL,
          ...)
    }
  } else {
    createInput <- function(id, i, elementName) {
      FUN(paste0(id, elementName),
          value = if (!is.null(initVals)) initVals[i] else NULL,
          label = if (!is.null(initLabels)) initLabels[i] else NULL,
          icon = if (!is.null(initIcons)) icon(initIcons[i], class = btnCLASS_ICON) else NULL,
          style = if (!is.null(initStyles)) initStyles[i] else NULL,
          selected = if (!is.null(initSelected)) initSelected[i] else NULL,
          onclick = if (!is.null(initOnclick)) initOnclick[i] else NULL,
          class = if (!is.null(initClasses)) initClasses[i] else NULL,
          ...)
    }
  }
  
  # go through number of elements
  for (i in seq(startFrom, length(postfix))) {
    if (!is.null(initDisabled) &&
      initDisabled[i] == TRUE) {
      inputs[i] <- as.character(
        disabled(createInput(id, i, postfix[i]))
      )
    } else {
      inputs[i] <- as.character(createInput(id, i, postfix[i]))
    }
  }
  
  # push back inputs
  inputs
}

# create select input
createSelectInput <- function(
  id, label = NULL, choices = list(),
  size = 1, multiple = FALSE, selectize = FALSE,
  selected = NULL) {
  # set size for selection
  selectSize <- 1
  
  if (!is.null(choices)) {
    if (length(choices) > size)
      selectSize <- size
    else 
      selectSize <- length(choices)
  }
  
  # skip size and selectize if size == 1
  if (selectSize <= 1) {
    return(
      selectInput(
        id,
        label,
        choices = choices,
        multiple = multiple,
        selected = selected
      )
    )
  } else {
    return(
      selectInput(
        id,
        label,
        choices = choices,
        multiple = multiple,
        size = selectSize, selectize = selectize,
        selected = selected
      )
    )
  }
}

# define default value
shinyInputValue <- function(id, input, value = NULL, ignoreInput = FALSE) {
  if (ignoreInput == FALSE && !is.null(input[[id]]))
    input[[id]]
  else
    value
}

# create shiny inputs from ID
getListOfShinyInputs <- function(input, inputID){
  inputs <- NULL
  
  # get inputs
  inputNames <- names(input)[grepl(pattern = inputID, names(input))]
  
  if (length(inputNames) > 0){
    inputs <- c()
    
    for (i in seq(length(inputNames))){
      inputs <- c(inputs, input[[paste0(inputID, i)]])
    }
  }
  
  return(inputs)
}

# convert hex colour to rgb
convertHexToRgba <- function(hexCode, opacity) {
  rgbaCode <- sprintf(
    "rgba(%s, %f)",
    paste0(col2rgb(hexCode), collapse=", "), opacity
  )
  
  return(rgbaCode)
}


## Helper functions for shiny files
# Join selected path
joinSelectedPath <- function(selection, useConfigVolumes = FALSE) {
  joinShinyFileSelection(selection, "path", useConfigVolumes)
}

# Join selected file
joinSelectedFile <- function(selection, useConfigVolumes = FALSE) {
  joinShinyFileSelection(selection, "files", useConfigVolumes)
}

# Join selected files
joinSelectedFiles <- function(selection, useConfigVolumes = FALSE) {
  joinShinyFileSelection(selection, "files", useConfigVolumes)
}

joinShinyFileSelection <- function(
  selection, type, useConfigVolumes) {
  
  # get system info
  osSystem <- Sys.info()["sysname"]
  
  # get root
  if (useConfigVolumes == TRUE){
    curRoot <- cciaConf()$volumes[[selection$root]]
  } else {
    if (osSystem == "Darwin") {
      curRoot <- file.path("/Volumes", selection$root[[1]])
    } else if (osSystem == "Linux") {
      curRoot <- selection$root[[1]]
    } else if (osSystem == "Windows") {
      # ignore Windows ...
    }
  }
  
  # get full path
  if (type == "files") {
    curPath <- lapply(selection[[type]], unlist)
    curPath <- lapply(curPath, paste, collapse = .Platform$file.sep)
    curPath <- paste(curRoot, curPath, sep = .Platform$file.sep)
  } else {
    curPath <- paste(selection[[type]], collapse = .Platform$file.sep)
    curPath <- file.path(curRoot, curPath)
  }
  
  # adjust path for Docker
  if (osSystem == "Linux") {
    # start from first '/' and replace '//'
    curPath <- stringr::str_replace(stringr::str_match(
      curPath, "/.*"), "//", "/")
  }
  
  # normalise
  curPath <- normalizePath(curPath)
  
  curPath
}

#' @description Trim away ns name from session input
#' @param sessionID character of session ID
#' @param inputID character of input ID
#' @examples
#' TODO
#' @export
trimInputName <- function(sessionID, inputID) {
  stringr::str_split(inputID, paste0(sessionID, "-"))[[1]][[2]]
}

## Bookmarking
# build state url
buildShinyBookmarkURL <- function(session, stateID) {
  url <- paste0("_state_id_=", encodeURIComponent(stateID))
  
  clientData <- session$clientData
  url <- paste0(
    clientData$url_protocol, "//",
    clientData$url_hostname,
    if (nzchar(clientData$url_port)) paste0(":", clientData$url_port),
    clientData$url_pathname,
    "?", url
  )
  
  url
}

# get shiny app location
getShinyAppPath <- function(root = FALSE, superroot = FALSE) {
  retVal <- getShinyOption("appDir")
  
  # go to root
  if (root == TRUE) {
    retVal <- file.path(retVal, "..")
  } else if (superroot == TRUE) {
    retVal <- file.path(retVal, "..", "..")
  }
  
  retVal
}

# get state file file
getShinyBookmarkStatePath <- function(stateID, bookmarkStateDir = NULL) {
  # get bookmark directory
  if (is.null(bookmarkStateDir)) {
    bookmarkStateDir <- getShinyAppPath(superroot = TRUE)
  }
  
  # search for the state file
  curDirs <- list.dirs(bookmarkStateDir, recursive = TRUE)
  
  # check which matches the stateID
  curFile <- curDirs[!is.na(stringr::str_match(curDirs, stateID))]
  
  curFile
}

# unlink bookmark
unlinkShinyBookmark <- function(stateID) {
  # get state path
  curFile <- getShinyBookmarkStatePath(stateID)
  
  if (!is.null(curFile)) {
    unlink(curFile, recursive = TRUE)
  }
}

## IDs
# generate unique ID
genUID <- function(uIDLength, numValues = 1){
  stri_rand_strings(numValues, uIDLength)
}

## Checking inputs
# https://stackoverflow.com/questions/43195519/check-if-string-contains-only-numbers-or-only-characters-r
checkForLettersOnly <- function(x) !grepl("[^A-Za-z]", x)
checkForNumbersOnly <- function(x) !grepl("\\D", x)

# ensure that the value is a list
forceAsFlatList <- function(val) {
  # check if value is one
  if (length(val) == 1) {
    val <- list(val)
  } else {
    val <- unlist(val)
  }
  
  val
}

# get reactive values starting with
reactiveValuesToListStartsWith <- function(reactVal, pattern){
  reactValList <- reactiveValuesToList(reactVal)
  reactValList <- reactValList[startsWith(names(reactValList), pattern)]
  
  reactValList
}

# observe event pattern
# https://stackoverflow.com/a/51622007/13766165
observeEventPattern <- function(id, pattern, input) {
  eventNames <- paste0(id, pattern)
  
  events <- lapply(paste0(id, pattern), function(x) input[[x]])
  names(events) <- eventNames
  
  events
}

# observe inputs
# https://stackoverflow.com/a/51622007/13766165
observeInputs <- function(ids, input, includeNULL = TRUE, inlcudeEmpty = TRUE) {
  events <- lapply(ids, function(x) input[[x]])
  names(events) <- ids
  
  # exclude null
  if (includeNULL == FALSE) 
    events <- events[lengths(events) > 0]
  
  # exclude empty
  if (inlcudeEmpty == FALSE) 
    events <- events[events != ""]
  
  events
}

# observe plotly event list
observePlotlyEventList <- function(event, sources = c()) {
  obsList <- list()
  sourceNames <- c()
  
  counter <- 1
  for (x in sources) {
    # get event data
    curDat <- event_data(event, source = x)
    
    # add to lists
    if (!is.null(curDat)) {
      obsList[[counter]] <- curDat
      sourceNames <- c(sourceNames, x)
      
      counter <- counter + 1
    }
  }
  
  names(obsList) <- sourceNames
  
  obsList
}

## Plotly styles
# change grid colour
plotlyAx <- list(
  zeroline = TRUE,
  showline = TRUE,
  gridwidth = 1,
  zerolinewidth = 2,
  linewidth = 1,
  gridcolor = "#222",
  zerolinecolor = "#999",
  # linecolor = "#999",
  title = list(text = ""),
  tickfont = list(color = "#d3d3d3")
  # ticks = "",
  # showticklabels = FALSE
)

# remove mode buttons
plotlyModeBarButtonsToRemove <- list(
  # "select2d", "lasso2d",
  "toggleSpikelines",
  "hoverClosestGl2d", "hoverClosestPie",
  "hoverClosestCartesian", "hoverCompareCartesian"
)

# icon as HTML
htmlIcon <- function(iconName, btnClass = btnCLASS_ICON) {
  sprintf(
    '<i class="fa fa-%s %s" role="presentation" aria-label="%s icon"></i>',
    iconName, btnClass, iconName
  )
}

# toggle button class
toggleButtonBoolean <- function(
  id, checkRes, classTRUE, classFALSE, btnLabels = NULL) {
  # switch classes if result is false
  if (checkRes == FALSE) {
    classTMP <- classFALSE
    classFALSE <- classTRUE
    classTRUE <- classTMP
  }
  
  # toggle classes
  removeClass(id, classFALSE)
  addClass(id, classTRUE)
  
  # change button labels
  if (!is.null(btnLabels)) {
    if (checkRes == TRUE) {
      html(id, btnLabels$success)
    } else {
      html(id, btnLabels$fail)
    }
  }
}

### -- ARE THE BELOW STILL NEEDED? --
paramUIName <- function(taskFunctionName, param) {
  # add function name and capitalise first letter
  paste0(
    taskFunctionName,
    toupper(substr(param, 1, 1)),
    substring(param, 2))
}

# build sliders with list
paramChoiceSliders <- function(
  session, uiMapping, taskFunctionName, funParams) {
  # build sliders
  uiElements <- list()
  
  paramCounter <- 1
  for (curParam in uiMapping) {
    uiElements[[paramCounter]] <- list(
      column(4, tags$label(curParam$label)),
      column(
        8,
        sliderInput(
          session$ns(paramUIName(taskFunctionName, curParam$name)), NULL,
          min = as.double(curParam$min),
          max = as.double(curParam$max),
          value = (if (curParam$name %in% names(funParams))
            funParams[[curParam$name]]
            else
              as.double(curParam$default)),
          step = as.double(curParam$step))
      )
    )
    
    paramCounter <- paramCounter + 1
  }
  
  # https://community.rstudio.com/t/how-to-position-multiple-dynamically-added-ui-elements-in-shiny-r/4036/2
  uiFunParams <- list()
  if (length(uiElements) > 0) {
    uiFunParams <- fluidRow(
      do.call(tagList, unlist(uiElements, recursive = FALSE))
    )
  }
  
  uiFunParams
}

# build selectionbox choices
channelChoiceBoxes <- function(
  session, channelNames, channelTypes, taskFunctionName, funParams,
  selectionSize = 5, channelMultiple = TRUE, channelWidth = 6) {
  uiElements <- list()
  
  # build channel choices
  namesChannelChoices <- channelNames
  
  # for processing, start counting channels at '0'
  channelChoices <- seq(length(namesChannelChoices)) - 1
  names(channelChoices) <- namesChannelChoices
  
  # set size for selection
  selectSize <- if(
    length(channelChoices) > selectionSize)
    cciaConf()$parameters$parameters$channelSelectionSize
  else length(channelChoices)
  
  paramCounter <- 1
  for (curType in channelTypes) {
    uiElements[[paramCounter]] <- list(
      column(
        channelWidth,
        selectInput(
          session$ns(paramUIName(taskFunctionName, curType[2])),
          curType[1],
          choices = channelChoices,
          multiple = channelMultiple,
          size = selectSize, selectize = FALSE,
          selected = (
            if (curType[2] %in% names(funParams))
              funParams[[curType[2]]]
            else
              NULL)
        )
      )
    )
    
    paramCounter <- paramCounter + 1
  }
  
  fluidRow(
    do.call(tagList, unlist(uiElements, recursive = FALSE))
  )
}
