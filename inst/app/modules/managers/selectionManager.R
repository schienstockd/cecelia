# Selection Manager
createSelectionManager <- function(
  input, output, session, globalManagers, moduleManagers, managerConf) {
  ### Reactive values
  selectedUIDs <- reactiveVal()
  selectedUIDRange <- reactiveVal()
  hotkeyPressed <- reactiveVal(FALSE)
  id <- if ("selectionID" %in% names(managerConf)) managerConf$selectionID else "image"
  
  ### Reactive-like values
  
  ### Reactives - RxCalc
  ## Event specific
  
  # listen to row toggle
  toggleRows <- eventReactive(input[[paste0(id, "ToggleRows")]], {
    input[[paste0(id, "ToggleRows")]]
  })
  
  # listen to item selection
  selectedUID <- eventReactive(input[[paste0(id, "SelectRow")]], {
    input[[paste0(id, "SelectRow")]]
  })
  
  # number of selected IDs
  numSelectedUIDs <- eventReactive(selectedUIDs(), {
    length(selectedUIDs())
  })
  
  ## Generic
  
  ### Observers - RxAction
  ## Event specific
  
  # toggle button symbols
  observeEvent(selectedUIDs(), {
    # tick buttons
    for (i in selectedUIDs()) {
      addClass(paste0(id, "SelectRow_", i), btnCLASS_SELECTED)
    }
    
    # get on selected rows
    if (any(!(managerConf$selectionData()$uID %in% selectedUIDs()))) {
      nonSelectedUIDs <- managerConf$selectionData()[
        !(managerConf$selectionData()$uID %in% selectedUIDs()), c("uID")]
      
      # untick buttons
      for (i in nonSelectedUIDs) {
        removeClass(paste0(id, "SelectRow_", i), btnCLASS_SELECTED)
      }
    }
  })
  
  # # set shift key
  # observeEvent(input$selectionHotkeys, {
  #   req(input$selectionHotkeys)
  #   
  #   hotkeyPressed(TRUE)
  # })
  
  # observe range selection with hotkeys
  observeEvent(input[[paste0(id, "SelectRow")]], {
    req(selectedUID())
    idRange <- NULL
    inputRow <- input[[paste0(id, "SelectRow")]]
    
    # was hotkey pressed?
    if (hotkeyPressed() == TRUE) {
      # first row selected?
      if (is.null(selectedUIDRange())) {
        idRange <- c(inputRow)
      } else {
        # get range
        rowA <- which(
          managerConf$selectionData()$uID == selectedUIDRange()[[1]])
        rowB <- which(
          managerConf$selectionData()$uID == inputRow)
        
        # return selected uIDs
        idRange <- managerConf$selectionData()[seq(rowA, rowB),]$uID
      }
      
      hotkeyPressed(FALSE)
      
      selectedUIDRange(idRange)
    } else {
      selectedUIDRange(NULL)
    }
  })
  
  # observe range selection
  observeEvent(c(
    selectedUIDRange()
  ), {
    # overwrite selection
    selectedUIDs(selectedUIDRange())
  })
  
  # single selection
  observeEvent(selectedUID(), {
    # toggle selection
    if (selectedUID() %in% selectedUIDs()) {
      selectedUIDs(selectedUIDs()[selectedUIDs() != selectedUID()])
    } else {
      selectedUIDs(c(selectedUIDs(), selectedUID()))
    }
  })
  
  # toggle rows
  observeEvent(toggleRows(), {
    if (length(selectedUIDs()) > 0) {
      selectedUIDs(list())
    } else {
      selectedUIDs(managerConf$selectionData()$uID)
    }
  })
  
  ### Functions
  
  # Create columns for selection management
  createSelectionColumn <- function() {
    # which rows are selected?
    isolate({
      selectRowClasses <- rep(NULL, nrow(managerConf$selectionData()))
      # selectDownClasses <- rep(NULL, nrow(managerConf$selectionData()))
      # selectUpClasses <- rep(NULL, nrow(managerConf$selectionData()))
      
      selectRowClasses[which(managerConf$selectionData()$uID %in% selectedUIDs())] <- btnCLASS_SELECTED
      
      # if (!is.null(input$selectDown)) {
      #   rowNum <- which(managerConf$selectionData()$uID == input$selectDown)
      #   selectDownClasses[rowNum] <- btnCLASS_SELECTED
      # }
      # 
      # if (!is.null(input$selectUp)) {
      #   rowNum <- which(managerConf$selectionData()$uID == input$selectUp)
      #   selectUpClasses[rowNum] <- btnCLASS_SELECTED
      # }
    })
    
    tableCols <- list(
      # " " = shinyInput(
      #   actionButton, session$ns("selectDown_"), managerConf$selectionData()$uID,
      #   initLabels = rep(btnLABEL_DOWN, nrow(managerConf$selectionData())),
      #   initClasses = selectDownClasses,
      #   initOnclick = paste(
      #     sprintf(
      #       'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
      #       session$ns("selectDown"),
      #       managerConf$selectionData()$uID
      #     )
      #   )
      # ),
      # " " = shinyInput(
      #   actionButton, session$ns("selectUp_"), managerConf$selectionData()$uID,
      #   initLabels = rep(btnLABEL_UP, nrow(managerConf$selectionData())),
      #   initClasses = selectUpClasses,
      #   initOnclick = paste(
      #     sprintf(
      #       'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
      #       session$ns("selectUp"),
      #       managerConf$selectionData()$uID
      #     )
      #   )
      # ),
      " " = shinyInput(
        "actionButton", session$ns(paste0(id, "SelectRow_")), managerConf$selectionData()$uID,
        initIcons = rep(btnICON_SELECTED, nrow(managerConf$selectionData())),
        initClasses = selectRowClasses,
        initOnclick = paste(
          sprintf(
            'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
            session$ns(paste0(id, "SelectRow")),
            managerConf$selectionData()$uID
          )
        )
      )
    )
    
    # set names for cols
    names(tableCols) <- c(
      as.character(actionLink(
        session$ns(paste0(id, "ToggleRows")), NULL, icon = icon("toggle-on"),
        style = "color:white;font-size:1.5em",
        onclick = paste(
          sprintf(
            'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
            session$ns(paste0(id, "ToggleRows")),
            runif(1)
            )
          )
        ))
    )

    tableCols
  }
  
  ## public functions
  list(
    createSelectionColumn = createSelectionColumn,
    # reactives
    selectedUIDs = selectedUIDs,
    numSelectedUIDs = numSelectedUIDs
  )
}
