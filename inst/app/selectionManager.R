# Selection Manager
createSelectionManager <- function(
  input, output, session, globalManagers, moduleManagers, managerConf) {
  ### Reactive values
  selectedUIDs <- reactiveVal()
  selectedUIDRange <- reactiveVal()
  hotkeyPressed <- reactiveVal(FALSE)
  
  ### Reactive-like values
  
  ### Reactives - RxCalc
  ## Event specific
  
  # listen to row toggle
  toggleRows <- eventReactive(input$toggleRows, {
    input$toggleRows
  })
  
  # listen to item selection
  selectedUID <- eventReactive(input$selectRow, {
    input$selectRow
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
      shinyjs::addClass(
        paste0("selectRow_", i), btnCLASS_SELECTED
      )
    }
    
    # get on selected rows
    if (any(!(managerConf$imageData()$uID %in% selectedUIDs()))) {
      nonSelectedUIDs <- managerConf$imageData()[
        !(managerConf$imageData()$uID %in% selectedUIDs()), c("uID")]
      
      # untick buttons
      for (i in nonSelectedUIDs) {
        shinyjs::removeClass(
          paste0("selectRow_", i), btnCLASS_SELECTED
          )
      }
    }
  })
  
  # set shift key
  observeEvent(input$selectionHotkeys, {
    req(input$selectionHotkeys)
    
    hotkeyPressed(TRUE)
  })
  
  # observe range selection with hotkeys
  observeEvent(input$selectRow, {
    req(selectedUID())
    idRange <- NULL
    
    # was hotkey pressed?
    if (hotkeyPressed() == TRUE) {
      # first row selected?
      if (is.null(selectedUIDRange())) {
        idRange <- c(input$selectRow)
      } else {
        # get range
        rowA <- which(
          managerConf$imageData()$uID == selectedUIDRange()[[1]])
        rowB <- which(
          managerConf$imageData()$uID == input$selectRow)
        
        # return selected uIDs
        idRange <- managerConf$imageData()[seq(rowA, rowB),]$uID
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
      selectedUIDs(managerConf$imageData()$uID)
    }
  })
  
  ### Functions
  
  # Create columns for selection management
  createSelectionColumn <- function() {
    # which rows are selected?
    isolate({
      selectRowClasses <- rep(NULL, nrow(managerConf$imageData()))
      selectDownClasses <- rep(NULL, nrow(managerConf$imageData()))
      selectUpClasses <- rep(NULL, nrow(managerConf$imageData()))
      
      selectRowClasses[which(managerConf$imageData()$uID %in% selectedUIDs())] <- btnCLASS_SELECTED
      
      if (!is.null(input$selectDown)) {
        rowNum <- which(managerConf$imageData()$uID == input$selectDown)
        selectDownClasses[rowNum] <- btnCLASS_SELECTED
      }
      
      if (!is.null(input$selectUp)) {
        rowNum <- which(managerConf$imageData()$uID == input$selectUp)
        selectUpClasses[rowNum] <- btnCLASS_SELECTED
      }
    })
    
    tableCols <- list(
      # " " = shinyInput(
      #   actionButton, session$ns("selectDown_"), managerConf$imageData()$uID,
      #   initLabels = rep(btnLABEL_DOWN, nrow(managerConf$imageData())),
      #   initClasses = selectDownClasses,
      #   initOnclick = paste(
      #     sprintf(
      #       'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
      #       session$ns("selectDown"),
      #       managerConf$imageData()$uID
      #     )
      #   )
      # ),
      # " " = shinyInput(
      #   actionButton, session$ns("selectUp_"), managerConf$imageData()$uID,
      #   initLabels = rep(btnLABEL_UP, nrow(managerConf$imageData())),
      #   initClasses = selectUpClasses,
      #   initOnclick = paste(
      #     sprintf(
      #       'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
      #       session$ns("selectUp"),
      #       managerConf$imageData()$uID
      #     )
      #   )
      # ),
      " " = shinyInput(
        "actionButton", session$ns("selectRow_"), managerConf$imageData()$uID,
        initIcons = rep(btnICON_SELECTED, nrow(managerConf$imageData())),
        initClasses = selectRowClasses,
        initOnclick = paste(
          sprintf(
            'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
            session$ns("selectRow"),
            managerConf$imageData()$uID
          )
        )
      )
    )
    
    # set names for cols
    names(tableCols) <- c(
      as.character(actionLink(
        session$ns("toggleRows"), NULL, icon = icon("toggle-on"),
        style = "color:white;font-size:1.5em",
        onclick = paste(
          sprintf(
            'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
            session$ns("toggleRows"),
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
