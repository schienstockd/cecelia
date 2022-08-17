createShapesManager <- function(
  input, output, session, globalManagers, moduleManagers, managerConf) {
  
  ### Reactive values
  shapesTable <- reactiveVal()
  shapesTableColumns <- reactiveVal()
  updateShapesTable <- reactiveVal()
  
  ### Reactive-like values
  
  ### Reactives - RxCalc
  ## Event specific
  
  ## Generic
  # ccia Object
  cciaObj <- reactive({
    managerConf$cciaObj()
  })
  
  # get shapes data
  shapesData <- reactive({
    req(cciaObj())
    
    # TODO this is very naive and not reactive
    cciaObj()$imShapes()
  })
  
  # should the image be updated automatically?
  autoUpdateImage <- reactive({
    TRUE
  })
  
  # save shapes image
  observeEvent(input$saveShapes, {
    req(cciaObj())
    req(moduleManagers()$imageViewerManager$imageShown())
    
    # save shapes
    globalManagers$viewerManager()$viewer()$saveShapes(
      execInteractive = FALSE
    )
  })
  
  # update image
  observeEvent(c(
    input$updateImage,
    managerConf$population$updateImage(),
    moduleManagers()$imageViewerManager$imageShown()
    ), {
    req(cciaObj())
    
    # do not show if the image has not been loaded yet
    req(moduleManagers()$imageViewerManager$imageShown())
    
    # show shapes
    if (globalManagers$viewerManager()$getShowShapes() == TRUE) {
      globalManagers$viewerManager()$viewer()$showShapes(
        execInteractive = TRUE
      )
    }
  })
  
  # generate population table
  observeEvent(c(
    shapesData()
  ), {
    req(cciaObj())
    
    # reset pop table if data empty but the
    # user added populations before
    if (is.null(shapesData()) && !is.null(shapesTableColumns())) {
      shapesTable(shapesTableColumns())
    }
    
    req(shapesData())
    
    shapesTableDF <- NULL
    if (length(shapesData()) > 0) {
      # create colour and delete column
      shapeCols <- list(
        "Shape" = shapesData()
      )
      
      # create dataframe
      shapesTableDF <- do.call(cbind, shapeCols)
      
      # add names
      # colnames(shapesTableDF) <- names(popCols)
      # rownames(shapesTableDF) <- rownames(popCols)
    } 
    
    # set table columns
    if (!is.null(shapesTableDF)) {
      if (is.null(shapesTableColumns()) || shapesTableColumns() != colnames(shapesTableDF)) {
        cols <- as.data.frame(
          rep(list(""), length(colnames(shapesTableDF))))
        colnames(cols) <- colnames(shapesTableDF)
        
        shapesTableColumns(cols)
      }
    } else {
      shapesTableColumns(NULL)
    }
    
    # set table
    shapesTable(shapesTableDF)
  }, ignoreNULL = FALSE)
  
  ## Generic
  
  ### UI Outputs
  ## Tables
  # shapes
  output$shapesTable <- DT::renderDataTable({
    req(shapesTableColumns())
    
    # get table
    options = list(
      fixedColumns = list(leftColumns = 1),
      fixedHeader = TRUE)
    
    tableOpts <- list(
      ordering = TRUE,
      dom = "tip",
      pageLength = 10
    )
    
    moduleManagers()$uiManager$dataTable(
      shapesTableColumns(), options = options, rownames = TRUE, editable = TRUE,
      ordering = tableOpts$ordering, pageLength = tableOpts$pageLength, dom = tableOpts$dom)
  })
  
  # update table without triggering redraw
  observeEvent(c(
    shapesTable(), updateShapesTable()
    ), {
    req(shapesTable())
    
    # https://stackoverflow.com/a/56879871/13766165
    replaceData(dataTableProxy("shapesTable"),
                shapesTable(),
                resetPaging = FALSE, rownames = FALSE)
  })
  
  ## Plots
  
  ## Buttons
  
  ## Other
  
  ## public functions
  list(
    autoUpdateImage = autoUpdateImage
  )
}
