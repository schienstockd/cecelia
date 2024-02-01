# Generate and manage inputs based on definition files
InputManager <- R6::R6Class(
  "InputManager",
  private = list(
    session = NULL,
    input = NULL,
    globalManagers = NULL,
    sourceDirectory = NULL,
    handleSelectedFun = NULL,
    inputDefinitions = list(),
    handleCciaObj = NULL,
    
    # handles for current input definitions
    uiDefs = NULL,
    specDefs = NULL,
    
    # population inputs
    popType = NULL,
    popTypeInputs = c(),
    
    # set to save and retrieve function parameters
    handleCciaObjSet = NULL,
    handleCciaObjCollection = NULL,
    
    # function inputs
    handleFunInputs = NULL,
    
    # return json element
    jsonElement = function(key) {
      elmnt <- list()
      
      # set names from fun
      elmntNames <- NULL
      if (key != "fun") {
        elmntNames <- names(self$funNames())
      }
      
      counter <- 1
      for (x in private$getInputDefinitions()) {
        if (!is.null(elmntNames)) {
          elmnt[[elmntNames[[counter]]]] <- x[[key]][[1]]
        } else {
          elmnt[[names(x[[key]])[[1]]]] <- x[[key]][[1]]
        }
        
        counter <- counter + 1
      }
      
      elmnt
    },
    
    # get json type
    jsonType = function(uiDef, defName) {
      if (!is.null(uiDef)) {
        # get name
        typeID <- names(uiDef)[startsWith(names(uiDef), defName)]
        typeName <- stringr::str_split(typeID, ":")[[1]][[2]]
        
        # get content
        typeContent <- uiDef[[typeID]]
        
        # put into list
        typeList <- list()
        typeList[[typeName]] <- typeContent
        
        typeList
      }
    },
    
    # get ui type
    uiType = function(uiDef) {
      private$jsonType(uiDef, "widget")
    },
    
    # get spec type
    specType = function(uiDef) {
      private$jsonType(uiDef, "type")
    },
    
    # convert to spec type
    convertToSpecType = function(x, specType) {
      typeName <- names(specType)
      
      # convert
      if (names(specType) %in% c("integer", "double", "character", "numeric")) {
        convFun <- eval(parse(text = sprintf("as.%s", typeName)))
        x <- convFun(x)
      }
        
      x
    },
    
    # setters
    setInput = function(x) {
      private$input <- x
    },
    
    setSession = function(x) {
      private$session <- x
    },
    
    setSourceDirectory = function(x) {
      private$sourceDirectory <- x
    },
    
    setGlobalManagers = function(x) {
      private$globalManagers <- x
    },
    
    addInputDefinition = function(key, x) {
      private$inputDefinitions[[key]] <- x
    },
    
    setPopTypeInputs = function(x) {
      private$popTypeInputs <- x
    },
    
    setUiDefs = function(x) {
      private$uiDefs <- x
    },
    
    setSpecDefs = function(x) {
      private$specDefs <- x
    },
    
    # getters
    getInput = function() {
      private$input
    },
    
    getSession = function() {
      private$session
    },
    
    getSourceDirectory = function() {
      private$sourceDirectory
    },
    
    getGlobalManagers = function() {
      private$globalManagers
    },
    
    getInputDefinitions = function() {
      private$inputDefinitions
    },
    
    getInputDefinition = function(key) {
      private$inputDefinitions[[key]]
    },
    
    getUiDefs = function() {
      private$uiDefs
    },
    
    getSpecDefs = function() {
      private$specDefs
    }
  ),
  
  public = list(
    # init
    initialize = function(input, session, sourceDirectory, globalManagers) {
      private$setInput(input)
      private$setSession(session)
      private$setSourceDirectory(sourceDirectory)
      private$setGlobalManagers(globalManagers)
      
      # init definitions
      self$initFunDefinitions()
    },
    
    # go through source directory and load definition files
    initFunDefinitions = function() {
      defFiles <- list.files(
        private$getSourceDirectory(), pattern = ".json", full.names = TRUE)
      
      # go through files and add definitions to list
      for (curFile in defFiles) {
        private$addInputDefinition(
          tools::file_path_sans_ext(basename(curFile)),
          jsonlite::fromJSON(readLines(curFile))
        )
      }
    },
    
    # return functions
    funNames = function() {
      private$jsonElement("fun")
    },
    
    # return specs
    funSpecs = function() {
      private$jsonElement("spec")
    },
    
    # return ui
    funUIs = function() {
      private$jsonElement("ui")
    },
    
    # get name for element
    uiElementName = function(elmntName, ns = TRUE) {
      curName <- paste(
        self$selectedFun(),
        elmntName,
        sep = "."
      )
      
      # convert to namespace?
      if (ns == TRUE) {
        curName <- private$getSession()$ns(curName)
      }
      
      curName
    },
    
    # get function parameter
    funParam = function(elmntName, defaultVal = NULL) {
      param <- NULL
      
      # if (!is.null(self$cciaObjectSet())) {
      if (!is.null(self$cciaObject())) {
        # get trimmed element name
        elmntNameTrimmed <- .trimModuleFunName(elmntName)
        
        # get function parameters from object first, then experimental set if not present
        params <- self$cciaObject()$moduleFunParams(self$selectedFun())
        if (is.null(params))
          params <- self$cciaObjectSet()$moduleFunParams(self$selectedFun())
        
        # grouped?
        if (any(!is.na(stringr::str_match(elmntNameTrimmed, "_")))) {
          elmntGroups <- unlist(stringr::str_split(elmntNameTrimmed, "_"))
          curParams <- params
          
          # TODO a bit more elegant .. ?
          for (x in elmntGroups) {
            if (x %in% names(curParams)) {
              curParams <- curParams[[x]]
            } else {
              curParams <- NULL
            }
          }
          
          param <- curParams
        } else {
          if (elmntNameTrimmed %in% names(params)) {
            param <- params[[elmntNameTrimmed]]
          }
        }
      } else if (elmntName %in% names(self$funParams())) {
        param <- self$funParams()[[elmntName]]
      }
      
      if (!is.null(param))
        param
      else
        defaultVal
    },
    
    # generate UI element
    createUIElement = function(elmntName, uiDef, specDef) {
      # get widget type
      uiType <- private$uiType(uiDef)
      specType <- private$specType(specDef)
      
      uiElement <- NULL
      
      elmntName <- self$uiElementName(elmntName)
      
      # build element
      if (names(uiType) == "slider") {
        uiElement <- self$createUISlider(
          elmntName, uiType, specType)
      } else if (names(uiType) == "sliderGroup") {
        uiElement <- self$createUISliderGroup(
          elmntName, uiType, specType)
      } else if (names(uiType) == "group") {
        uiElement <- self$createUIGroup(
          elmntName, uiType, specType)
      } else if (names(uiType) == "sliderImageIntensity") {
        uiElement <- self$createUISliderImageIntensity(
          elmntName, uiType, specType)
      } else if (names(uiType) == "sliderTimepoints") {
        uiElement <- self$createUISliderTimepoints(
          elmntName, uiType, specType)
      } else if (names(uiType) == "imageSourceSelection") {
        uiElement <- self$createUIImageSourceSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "valueNameSelection") {
        uiElement <- self$createUIValueNameSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "imageSetSelection") {
        uiElement <- self$createUIImageSetSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "imageSelection") {
        uiElement <- self$createUIImageSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "clSelection") {
        uiElement <- self$createUIClSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "clSelectionGroup") {
        uiElement <- self$createUIClSelectionGroup(
          elmntName, uiType, specType)
      } else if (names(uiType) == "modelSelectionGroup") {
        uiElement <- self$createUIModelSelectionGroup(
          elmntName, uiType, specType)
      } else if (names(uiType) == "channelSelection") {
        uiElement <- self$createUIChannelSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "channelSelectionTypeGroup") {
        uiElement <- self$createUIChannelSelectionTypeGroup(
          elmntName, uiType, specType)
      } else if (names(uiType) == "labelPropsColsSelection") {
        uiElement <- self$createUILabelPropsColsSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "channelCombinations") {
        uiElement <- self$createUIChannelCombinations(
          elmntName, uiType, specType)
      } else if (names(uiType) == "popSelection") {
        uiElement <- self$createUIPopSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "popSelectionGroup") {
        uiElement <- self$createUIPopSelectionGroup(
          elmntName, uiType, specType)
      } else if (names(uiType) == "shapesSelection") {
        uiElement <- self$createUIShapesSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "popTypeSelection") {
        uiElement <- self$createUIPopTypeSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "moduleFunSelection") {
        uiElement <- self$createUIModuleFunSelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "selection") {
        uiElement <- self$createUISelection(
          elmntName, uiType, specType)
      } else if (names(uiType) == "radioButtons") {
        uiElement <- self$createUIRadioButtons(
          elmntName, uiType, specType)
      } else if (names(uiType) == "textInput") {
        uiElement <- self$createUITextInput(
          elmntName, uiType, specType)
      } else if (names(uiType) == "checkbox") {
        uiElement <- self$createUICheckbox(
          elmntName, uiType, specType)
      } else if (names(uiType) == "channelGroup") {
        uiElement <- self$createUIChannelGroup(
          elmntName, uiType, specType)
      }
      
      # create ui
      if (endsWith(tolower(names(uiType)), "group")) {
        uiList <- list(
          fluidRow(column(12, tags$label(uiDef$label))),
          fluidRow(column(12, uiElement$ui))
        )
      } else {
        uiList <- list(
          column(4, tags$label(uiDef$label)),
          column(8, uiElement$ui)
        )
      }
      
      # add label
      list(
        ui = uiList,
        # convert names back
        names = sapply(uiElement$names, .trimModuleFunName,
                       USE.NAMES = FALSE),
        observers = if ("observers" %in% names(uiElement)) uiElement$observers else NULL
      )
    },
    
    # selection list
    createUISelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      list(
        ui = createSelectInput(
          elmntName,
          choices = uiContent$items,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(elmntName)
        ),
        names = elmntName
      )
    },
    
    # radio buttons
    createUIRadioButtons = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      uiDefault <- private$convertToSpecType(specContent$default, specType)
      
      list(
        ui = radioButtons(
          elmntName, NULL,
          choices = uiContent$items,
          selected = self$funParam(elmntName, uiDefault)
        ),
        names = elmntName
      )
    },
    
    # text input
    createUITextInput = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      uiDefault <- private$convertToSpecType(specContent$default, specType)
      
      list(
        ui = textInput(
          elmntName, NULL,
          value = self$funParam(elmntName, uiDefault)
        ),
        names = elmntName
      )
    },
    
    # checkbox
    createUICheckbox = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      uiDefault <- private$convertToSpecType(specContent$default, specType)
      
      list(
        ui = checkboxInput(
          elmntName, NULL,
          value = self$funParam(elmntName, uiDefault)
        ),
        names = elmntName
      )
    },
    
    # value name list for field
    createUIValueNameSelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # build field choices
      if ("valueType" %in% names(uiContent))
        valueNames <- self$cciaObject()$valueNames(
          uiContent$field, valueType = uiContent$valueType)
      else
        valueNames <- self$cciaObject()$valueNames(uiContent$field)

      list(
        ui = createSelectInput(
          elmntName,
          choices = valueNames,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(
            elmntName, defaultVal = attr(valueNames, "default"))
        ),
        names = elmntName
      )
    },
    
    # image source list
    createUIImageSourceSelection = function(elmntName, uiType, specType) {
      # add field
      uiType[[1]]$field <- "imFilepath"
      
      # call value name selection
      self$createUIValueNameSelection(elmntName, uiType, specType)
    },
    
    # image set list
    createUIImageSetSelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # build image set choices ..
      setChoices <- .reverseNamedList(
        private$getGlobalManagers()$dataManager()$cciaImageCollection()$cciaObjectNames())

      list(
        ui = createSelectInput(
          elmntName,
          choices = setChoices,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(elmntName)
        ),
        names = elmntName
      )
    },
    
    # image list
    createUIImageSelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # build image choices from set
      # TODO have a selection box to select
      # sets and then update the available images
      imageChoices <- names(self$cciaObjectSet()$cciaObjects())
      
      # add zero choice
      imageChoices <- self$addZeroChoiceToList(
        imageChoices, uiContent
      )
      
      names(imageChoices) <- imageChoices
      
      list(
        ui = createSelectInput(
          elmntName,
          choices = imageChoices,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(elmntName)
        ),
        names = elmntName
      )
    },
    
    # add choices to a list
    addChoicesToList = function(namesChoices, uiContent) {
      if ("addChoices" %in% names(uiContent)) {
        if (length(uiContent$addChoices) > 0) {
          # check that they are not already in the list
          addChoices <- uiContent$addChoices
          addChoices <- addChoices[!addChoices %in% namesChoices]
          
          namesChoices <- c(
            namesChoices,
            addChoices
          )
        }
      }
      
      namesChoices
    },
    
    # add zero choice to list
    addZeroChoiceToList = function(namesChoices, uiContent) {
      if ("addZeroChoice" %in% names(uiContent) &&
          uiContent$addZeroChoice == TRUE) {
        namesChoices <- c("", namesChoices)
        
        # add names
        if (length(namesChoices) > 1) {
          names(namesChoices) <- c("NONE", namesChoices[2:length(namesChoices)])
        } else {
          names(namesChoices) <- c("NONE")
        }
      }
      
      namesChoices
    },
    
    # label props cols selection
    createUILabelPropsColsSelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # add values to choices?
      namesChoices <- self$cciaObject()$labelPropsCols()
      namesChoices <- self$addChoicesToList(
        namesChoices, uiContent
      )
      
      # exclude channels?
      if ("includeChannels" %in% names(uiContent) && uiContent$includeChannels == FALSE) {
        namesChoices <- namesChoices[
          !namesChoices %in% self$cciaObject()$imChannelNames(
            correctChannelNames = TRUE, includeTypes = TRUE)]
      }
      
      list(
        ui = createSelectInput(
          elmntName,
          choices = namesChoices,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(elmntName)
        ),
        names = elmntName
      )
    },
    
    # classifier list
    createUIClSelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # build choices
      clChoices <- self$cciaObjectCollection()$clFiles(uiContent$type)
      
      list(
        ui = createSelectInput(
          elmntName,
          choices = clChoices,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(elmntName)
        ),
        names = elmntName
      )
    },
    
    # classifier list group
    createUIClSelectionGroup = function(elmntName, uiType, specType) {
      # get classifiers and walk through
      clChoices <- self$cciaObjectCollection()$clFiles(uiType[[1]]$type)
      
      # remove trailing '.cl'
      clChoices <- stringr::str_replace(clChoices, "\\.cl$", "")
      
      # set names to create list
      names(clChoices) <- clChoices
      
      # add classifiers as items
      uiType[[1]]$items <- clChoices
      
      # build group with classifiers
      self$createUIGroup(elmntName, uiType, specType)
    },
    
    # model list group
    createUIModelSelectionGroup = function(elmntName, uiType, specType) {
      # get classifiers and walk through
      modelChoices <- self$cciaObjectCollection()$modelFiles(uiType[[1]]$type)
      
      # set names to create list
      names(modelChoices) <- modelChoices
      
      # add classifiers as items
      uiType[[1]]$items <- modelChoices
      
      # build group with classifiers
      self$createUIGroup(elmntName, uiType, specType)
    },
    
    # channel list
    createUIChannelSelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      includeTypes <- FALSE
      
      # include types
      if ("includeTypes" %in% names(uiContent) && uiContent$includeTypes == TRUE) {
        includeTypes <- TRUE
      }
      
      # build channel choices
      namesChannelChoices <- self$cciaObject()$imChannelNames(includeTypes = includeTypes)
      
      # add values to choices?
      namesChannelChoices <- self$addChoicesToList(
        namesChannelChoices, uiContent
      )
      
      # add zero choice?  
      namesChannelChoices <- self$addZeroChoiceToList(
        namesChannelChoices, uiContent
      )
      
      # for processing, start counting channels at '0'
      channelChoices <- seq(length(namesChannelChoices)) - 1
      names(channelChoices) <- namesChannelChoices
      
      # use names instead of channel numbers
      if ("useNames" %in% names(uiContent) && uiContent$useNames == TRUE) {
        channelChoices <- namesChannelChoices
        names(channelChoices) <- namesChannelChoices
      }
      
      list(
        ui = createSelectInput(
          elmntName,
          choices = channelChoices,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(elmntName)
        ),
        names = elmntName
      )
    },
    
    # channel list grouped by type
    createUIChannelSelectionTypeGroup = function(elmntName, uiType, specType) {
      # get types and walk through to create channel type lists
      channelTypes <- c("base", attr(self$cciaObject()$imChannelNames(), "types"))
      
      # set names to create list
      names(channelTypes) <- channelTypes
      
      # add channels as items
      uiType[[1]]$items <- channelTypes
      
      # build group with channels
      self$createUIGroup(elmntName, uiType, specType)
    },
    
    # channel combinations
    createUIChannelCombinations = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # build channel combinations with a selection
      # field for each channel to be combined with anotherchoices
      namesChannelChoices <- self$cciaObject()$imChannelNames()
      
      # for processing, start counting channels at '0'
      channelChoices <- seq(length(namesChannelChoices)) - 1
      names(channelChoices) <- namesChannelChoices
      
      # go through all channels and build selection elements
      selectionList <- list()
      nameList <- c()
      
      for (x in names(channelChoices)) {
        i <- channelChoices[[x]]
        curElmntName <- sprintf("%s_%d", elmntName, i)
        
        # add to list
        selectionList[[x]] <- createSelectInput(
          curElmntName,
          x,
          choices = channelChoices,
          multiple = uiContent$multiple,
          selected = self$funParam(curElmntName)
        )
        
        nameList <- c(nameList, curElmntName)
      }
      
      list(
        ui = tagList(selectionList),
        names = nameList
      )
    },
    
    # channel group
    createUIChannelGroup = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # build channel combinations with a selection
      # field for each channel to be combined with another choices
      namesChannelChoices <- self$cciaObject()$imChannelNames()
      
      # for processing, start counting channels at '0'
      channelChoices <- as.character(seq(length(namesChannelChoices)) - 1)
      names(channelChoices) <- namesChannelChoices
      
      # add channels as items
      uiType[[1]]$items <- channelChoices
      
      # build group with channels
      self$createUIGroup(elmntName, uiType, specType)
    },
    
    # group
    createUIGroup = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # go through group items and build elements
      uiElements <- list()
      nameList <- c()
      toggleVis <- FALSE
      toggleDyn <- FALSE
      observerList <- list()
      
      # get items
      groupItems <- list()
      
      if ("items" %in% names(uiContent)) {
        # it could be a named list - so do not change
        # groupItems <- as.character(uiContent$items)
        groupItems <- uiContent$items
      } else if ("numItems" %in% names(uiContent)) {
        # build items
        groupItems <- as.character(seq(uiContent$numItems))
        names(groupItems) <- groupItems
      }
      
      # dynamic content
      if ("dynItems" %in% names(uiContent) && uiContent$dynItems == TRUE) {
        addRowName <- paste0(elmntName, "_addRow")
        
        print(paste(">> observe", addRowName))
        observerList[[addRowName]] <- expression({
          browser()
          # new_id <- paste("row", input$addLine, sep = "_")
          # insertUI(
          #   selector = "#placeholder",
          #   where = "beforeBegin",
          #   ui = row_ui(new_id)
          # )
          # 
          # handler_list <- isolate(handler())
          # new_handler <- callModule(row_server, new_id)
          # handler_list <- c(handler_list, new_handler)
          # names(handler_list)[length(handler_list)] <- new_id
          # handler(handler_list)
          # 
          # observeEvent(input[[paste0(new_id, '-deleteButton')]], {
          #   removeUI(selector = sprintf('#%s', new_id))
          #   remove_shiny_inputs(new_id, input)
          # })
        })
        
        toggleDyn <- TRUE
      }
      
      # toggle content
      if ("visible" %in% names(uiContent)) {
        visName <- paste0(elmntName, "_visibility")
        nameList <- append(nameList, visName)
        
        uiElements[["SPACER"]] <- fluidRow(
          # column(12, checkboxInput(visName, "Show all", uiContent$visible))
          column(4, checkboxInput(visName, "Show all", TRUE))
          # TODO not sure how to add oberservers here
          # if (toggleDyn == TRUE) column(2, actionButton(addRowName, "", icon = shiny::icon("plus"))) else NULL,
        )
        
        toggleVis <- TRUE
      }
      
      for (i in names(groupItems)) {
        x <- groupItems[[i]]
        xID <- sprintf("%s_%s", .trimModuleFunName(elmntName), x)
        
        uiGroupElements <- list()
        
        # go through elements
        for (j in names(uiContent)[!names(uiContent) %in% c("items", "numItems", "dynItems", "type", "visible")]) {
          curUI <- uiContent[[j]]
          curSpecs <- specContent[[j]]
          
          curElmntName <- sprintf("%s_%s", xID, j)
          
          # get element
          uiGroupElements[[j]] <- self$createUIElement(
            curElmntName, curUI, curSpecs
          )
          
          #add input to list
          nameList <- c(nameList, curElmntName)
        }

        # make panel dynamic?
        itemLabel <- tags$i(tags$label(i))
        
        # add vis
        if (toggleVis == TRUE) {
          visItemName <- paste0(visName, "_", x)
          
          itemLabel <- tags$i(checkboxInput(visItemName, i, uiContent$visible))
        }
        
        if (toggleDyn == TRUE) {
          itemLabel <- fluidRow(
            column(10, itemLabel)
            # column(2, actionButton(paste0(xID, "_del"), "", icon = shiny::icon("minus")))
          )
        }
        
        # add conditional panel
        # TODO is this too complicated here?
        elementRow <- tagList(
          fluidRow(column(12, itemLabel)),
          tags$div(id = xID, fluidRow(column(12, tagList(lapply(uiGroupElements, function(x) x$ui)))), tags$hr())
        )
        
        if (toggleVis == TRUE) {
          uiElements[[x]] <- tagList(
            elementRow[[1]],
            conditionalPanel(
              # condition = sprintf("input['%s'] == true", private$getSession()$ns(visName)),
              condition = sprintf("input['%s'] == true && input['%s'] == true",
                                  visName, visItemName),
              elementRow[[2]])
          )
        } else {
          uiElements[[x]] <- elementRow
        }
      }
      
      list(
        ui = tagList(uiElements),
        names = nameList,
        observers = observerList
      )
    },
    
    # module function list
    createUIModuleFunSelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # build choices
      choices <- private$getInputDefinitions()
      choices <- choices[!names(choices) %in% c("retrieve", "upload")]
      choicesNames <- sapply(
        choices, function(x) x$fun[[1]]$label)
      choices <- names(choices)
      names(choices) <- choicesNames
      
      list(
        ui = createSelectInput(
          elmntName,
          choices = choices,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(elmntName)
        ),
        names = elmntName
      )
    },
    
    # population type list
    createUIPopTypeSelection = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # build choices
      choices <- .reverseNamedList(cciaConf()$parameters$popTypes)
      
      list(
        ui = createSelectInput(
          elmntName,
          choices = choices,
          multiple = uiContent$multiple,
          size = uiContent$size, selectize = FALSE,
          selected = self$funParam(elmntName)
        ),
        names = elmntName
      )
    },
    
    # build population choices
    buildPopChoices = function(uiContent) {
      popChoices <- NULL
      includeRoot <- FALSE
      tracksOnly <- FALSE
      
      if ("includeRoot" %in% names(uiContent) && uiContent$includeRoot == TRUE)
        includeRoot <- TRUE
      
      if ("tracksOnly" %in% names(uiContent) && uiContent$tracksOnly == TRUE)
        tracksOnly <- TRUE
      
      if ("showAll" %in% names(uiContent) && uiContent$showAll == TRUE) {
        popChoices <- self$cciaObject()$popPathsAll(
          includeFiltered = TRUE, flattenPops = TRUE, includeRoot = includeRoot,
          useNames = !"useNames" %in% names(uiContent) || uiContent$useNames == FALSE,
          tracksOnly = tracksOnly)
      } else {
        # was a pop type specified?
        if ("popType" %in% names(uiContent))
          popType <- uiContent$popType
        else
          popType <- self$getPopType()
        
        if (!is.null(self$cciaObject()$popUtils(popType))) {
          popChoices <- self$cciaObject()$popPaths(
            popType, includeFiltered = TRUE, includeRoot = includeRoot, tracksOnly = tracksOnly)
          
          # use names instead of uIDs
          if (!"useNames" %in% names(uiContent) || uiContent$useNames == FALSE) {
            names(popChoices) <- popChoices
          }
        }
        
        # add zero choice
        popChoices <- self$addZeroChoiceToList(
          popChoices, uiContent
        )
        
        popChoices <- .reverseNamedList(popChoices)
      }
      
      popChoices
    },
    
    # population list
    createUIPopSelection = function(elmntName, uiType, specType = NULL) {
      uiContent <- uiType[[1]]
      
      if (!is.null(specType))
        specContent <- specType[[1]]
      
      # build pop choices
      popChoices <- self$buildPopChoices(uiContent)
      
      # add to population inputs
      private$setPopTypeInputs(
        c(self$getPopTypeInputs(), elmntName)
      )
      
      # this will result in a nicer select box
      ui <- createSelectInput(
        elmntName,
        choices = popChoices,
        multiple = uiContent$multiple,
        size = uiContent$size, selectize = FALSE,
        selected = self$funParam(elmntName)
      )
      
      list(
        ui = ui,
        names = elmntName
      )
    },
    
    # populations list group
    createUIPopSelectionGroup = function(elmntName, uiType, specType) {
      # get classifiers and walk through
      popChoices <- self$buildPopChoices(uiType[[1]])
      
      # set names to create list
      names(popChoices) <- popChoices
      
      # add classifiers as items
      uiType[[1]]$items <- popChoices
      
      # build group with classifiers
      self$createUIGroup(elmntName, uiType, specType)
    },
    
    # shapes list
    createUIShapesSelection = function(elmntName, uiType, specType = NULL) {
      uiContent <- uiType[[1]]
      
      if (!is.null(specType))
        specContent <- specType[[1]]
      
      # build shape choices
      shapesChoices <- NULL
      
      # TODO this will give the default shapes
      if (!is.null(self$cciaObject()$imShapes())) {
        shapesChoices <- self$cciaObject()$imShapes()
      }
      
      # this will result in a nicer select box
      ui <- createSelectInput(
        elmntName,
        choices = shapesChoices,
        multiple = uiContent$multiple,
        size = uiContent$size, selectize = FALSE,
        selected = self$funParam(elmntName)
      )
      
      list(
        ui = ui,
        names = elmntName
      )
    },
    
    # slider group
    createUISliderGroup = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # go through group and build sliders
      uiElements <- list()
      nameList <- c()
      for (i in names(specContent)) {
        curUI <- uiContent[[i]]
        curSpecs <- specContent[[i]][[1]]
        
        curElmntName <- sprintf("%s_%s", elmntName, i)
        
        # get specs
        uiMin <- private$convertToSpecType(curSpecs$min, specContent[[i]])
        uiMax <- private$convertToSpecType(curSpecs$max, specContent[[i]])
        uiDefault <- private$convertToSpecType(curSpecs$default, specContent[[i]])
        uiStep <- private$convertToSpecType(curUI$step, specContent[[i]])
        
        # build ui and add to list
        uiElements[[i]] <- fluidRow(
          column(
            3, tags$label(curUI$label)
          ),
          column(
            9,
            sliderInput(
              curElmntName, NULL,
              min = uiMin,
              max = uiMax,
              value = self$funParam(curElmntName, uiDefault),
              step = uiStep
            )
          )
        )
        nameList <- c(nameList, curElmntName)
      }
      
      list(
        ui = tagList(uiElements),
        names = nameList
      )
    },
    
    # slider timepoints
    createUISliderTimepoints = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # get pixel information
      pixels <- self$cciaObject()$omeXMLPixels()
      
      # start counting at '0'
      uiMin <- 0
      uiMax <- pixels$SizeT - 1
      
      # default is full timerange
      uiDefault <- c(0, uiMax)
      uiStep <- private$convertToSpecType(uiContent$step, specType)
      
      list(
        ui = sliderInput(
          elmntName, NULL,
          min = uiMin,
          max = uiMax,
          value = self$funParam(elmntName, uiDefault),
          step = uiStep
        ),
        names = elmntName
      )
    },
    
    # slider image intensity
    createUISliderImageIntensity = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      # get pixel information
      pixels <- self$cciaObject()$omeXMLPixels()
      
      # TODO ideally, this information would be taken from the image
      # a) load zarr with z5 wrapper
      # b) get values from selected image in napari and push
      #    back to shiny
      # c) simple version, get max from image datatype - not ideal
      uiMin <- 0
      uiMax <- 2^as.numeric(stringr::str_extract(pixels$Type, "[0-9]+"))
      uiDefault <- private$convertToSpecType(specContent$default, specType)
      uiStep <- private$convertToSpecType(uiContent$step, specType)
      
      list(
        ui = sliderInput(
          elmntName, NULL,
          min = uiMin,
          max = uiMax,
          value = self$funParam(elmntName, uiDefault),
          step = uiStep
        ),
        names = elmntName
      )
    },
    
    # slider
    createUISlider = function(elmntName, uiType, specType) {
      uiContent <- uiType[[1]]
      specContent <- specType[[1]]
      
      uiMin <- private$convertToSpecType(specContent$min, specType)
      uiMax <- private$convertToSpecType(specContent$max, specType)
      uiDefault <- private$convertToSpecType(specContent$default, specType)
      uiStep <- private$convertToSpecType(uiContent$step, specType)
      
      list(
        ui = sliderInput(
          elmntName, NULL,
          min = uiMin,
          max = uiMax,
          value = self$funParam(elmntName, uiDefault),
          step = uiStep
        ),
        names = elmntName
      )
    },
    
    # generate UI elements for function
    createFunParamsUI = function(funName) {
      # set current function and input
      self$setSelectedFun(funName)
      
      # reset population inputs
      private$setPopTypeInputs(c())
      
      # trim the name
      funTrimmed <- .trimModuleFunName(funName)
      
      uiMapping <- list()
      uiObservers <- list()
      
      # get UIs
      private$setUiDefs(NULL)
      private$setSpecDefs(NULL)
      
      if (funTrimmed %in% names(self$funUIs())) {
        private$setUiDefs(self$funUIs()[[funTrimmed]])
        private$setSpecDefs(self$funSpecs()[[funTrimmed]])
      }
      
      # create list of inputs as some elements
      # can produce more than one input element
      inputList <- c()
      
      if (!is.null(private$getUiDefs())) {
        # go through elements and generate UIs
        for (x in names(private$getUiDefs())) {
          uiElement <- self$createUIElement(
            x,
            private$getUiDefs()[[x]],
            private$getSpecDefs()[[x]]
            )
          
          # add to mapping
          uiMapping[[x]] <- uiElement$ui
          
          # add oberservers
          if ("observers" %in% names(uiElement))
            uiObservers[[x]] <- uiElement$observers
          
          # add inputs to list
          inputList <- c(inputList, uiElement$names)
        }
      }
      
      # set as input
      # self$setFunInputs(names(uiMapping))
      self$setFunInputs(inputList)
      
      # return UI
      # TODO not sure how to handle observers in UI
      list(
        ui = tagList(uiMapping),
        observers = uiObservers
      )
      # tagList(uiMapping)
    },
    
    # return ccia object for input
    cciaObject = function() {
      private$handleCciaObj
    },
    
    # return ccia object set for function parameters
    cciaObjectSet = function() {
      private$handleCciaObjSet
    },
    
    # return ccia object collection for function parameters
    cciaObjectCollection = function() {
      private$handleCciaObjCollection
    },
    
    # return current function
    selectedFun = function() {
      private$handleSelectedFun
    },
    
    # return inputs
    funInputs = function() {
      private$handleFunInputs
    },
    
    # return values for inputs
    funParams = function(trim = FALSE) {
      if (!is.null(self$funInputs())) {
        # get inputs
        curInput <- private$getInput()
        
        curIDs <- self$uiElementName(self$funInputs(), ns = FALSE)
        
        # get values
        curVals <- lapply(curIDs, function(x) curInput[[x]])
        
        # trim ids
        if (trim == TRUE) {
          curIDs <- lapply(curIDs, .trimModuleFunName)
        }
        
        names(curVals) <- curIDs
        
        # are there values that have to be combined into
        # named lists?
        # get group widgets
        groupWidgets <- self$widgetTypes()
        groupWidgets <- groupWidgets[endsWith(tolower(groupWidgets), "group")]
        
        for (i in names(groupWidgets)) {
          # add a new parameter and push
          # values as named list
          listItems <- stringr::str_extract(
            names(curVals), sprintf("(?<=%s_).+", i)
          )
          listItems <- listItems[!is.na(listItems)]
          
          # get names for list
          # listEntries <- stringr::str_split(listItems, "_")
          # https://stackoverflow.com/a/16753614
          listEntries <- unname(mapply(
            c,
            stringr::str_replace(listItems, "_[:alnum:]+$", ""),
            stringr::str_extract(listItems, "(?<=_)[:alnum:]+$"),
            SIMPLIFY = FALSE
            ))
          listNames <- unique(sapply(listEntries, function(x) x[[1]]))
          listKeys <- unique(sapply(listEntries, function(x) x[[2]]))
          
          # add new entries
          curVals[[i]] <- list()
          for (j in listNames) {
            curVals[[i]][[j]] <- list()
            
            # go through keys
            if (!all(is.na(listKeys))) {
              for (k in listKeys) {
                # curValName <- sprintf("%s_%s_%s", i, j, k)
                curValName <- sprintf("%s_%s_%s", i, j, k)
                
                curVals[[i]][[j]][[k]] <- if (!is.null(curVals[[curValName]]))
                  curVals[[curValName]] else list()
                curVals[[curValName]] <- NULL
              }
            } else {
              curValName <- sprintf("%s_%s", i, j)
              
              curVals[[i]][[j]] <- if (!is.null(curVals[[curValName]]))
                curVals[[curValName]] else list()
              curVals[[curValName]] <- NULL
            }
          }
        }
        
        curVals
      }
    },
    
    # get widget types for current function
    widgetTypes = function(widgetType = NULL) {
      # get widget types
      widgetTypes <- lapply(private$getUiDefs(),
             function(x) {
               widgetTypes <- stringr::str_extract(names(x), "(?<=widget:).*")
               widgetTypes[!is.na(widgetTypes)]
             })
      
      # filter for a specific type?
      if (!is.null(widgetType)) {
        widgetTypes <- widgetTypes[widgetTypes == widgetType]
      }
      
      widgetTypes
    },
    
    # setters
    setSelectedFun = function(x) {
      private$handleSelectedFun <- x
    },
    
    setCciaObject = function(x) {
      private$handleCciaObj <- x
    },
    
    setCciaObjectSet = function(x) {
      private$handleCciaObjSet <- x
    },
    
    setCciaObjectCollection = function(x) {
      private$handleCciaObjCollection <- x
    },
    
    setFunInputs = function(x) {
      private$handleFunInputs <- x
    },
    
    setPopType = function(x) {
      private$popType <- x
    },
    
    # getters
    getPopType = function() {
      private$popType
    },
    
    getPopTypeInputs = function() {
      private$popTypeInputs
    }
  )
)
