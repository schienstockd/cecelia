#' @description Shiny server
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @examples
#' TODO
server <- function(input, output, session) {
  ### Functions
  
  ### create global managers first
  globalManagers <- list()
  
  globalManagers$projectManager <- ProjectManager$new(session)$reactive()
  globalManagers$dataManager <- DataManager$new(session)$reactive()
  globalManagers$viewerManager <- ViewerManager$new(session)$reactive()
  
  # https://www.r-bloggers.com/2018/07/using-global-input-values-inside-of-r-shiny-modules/
  globalManagers$input <- list(sidebar = reactive(input$sidebar))
  
  # flag for init
  dataManagerInitialised <- reactiveVal(FALSE)
  
  # observe project path and init data manager once set
  observeEvent(globalManagers$projectManager()$getProjectPath(), {
    req(globalManagers$projectManager()$getProjectPath())
    req(dataManagerInitialised() == FALSE)
    
    # call init
    globalManagers$dataManager()$initCcciaCollections(
      globalManagers$projectManager()$persistentObjectDirectory()
    )
    
    # set flag
    dataManagerInitialised(TRUE)
  })
  
  ### continue ...
  # create version modal
  showCreateVersion <- function(failed = c()) {
    modalDialog(
      fluidRow(
        column(
          8,
          if ("createVersionComment" %in% failed) {
            tags$div(errCREATE_VERSION_ADD_COMMENT,
                     class = errCLASS)
            },
          textInput(
            session$ns("createVersionComment"),
            "Comment"),
          actionButton(
            session$ns("createVersionSubmit"),
            "Create Version")
          )
      ),
      easyClose = TRUE
    )
  }
  
  # export project
  showExportProject <- function(failed = c()) {
    modalDialog(
      fluidRow(
        column(
          8,
          if ("exportProjectPath" %in% failed) {
            tags$div("Enter correct path",
                     class = errCLASS)
          },
          textInput(session$ns("exportProjectPath"), "Directory to export"),
          # shinyDirButton(
          #   session$ns("exportProjectPath"), label = "Select directory",
          #   title = 'Select directory'
          # ),
          actionButton(
            session$ns("exportProjectSubmit"),
            "Export Project")
        )
      ),
      easyClose = TRUE
    )
  }
  
  # import project
  showImportProject <- function(failed = c()) {
    modalDialog(
      fluidRow(
        column(
          8,
          if ("importProjectPath" %in% failed) {
            tags$div("Enter correct path",
                     class = errCLASS)
          },
          # textInput(
          #   session$ns("importProjectPath"),
          #   "File to import"),
          shinyFilesButton(
            session$ns("importProjectPath"),
            "Select File", NULL, multiple = FALSE),
          actionButton(
            session$ns("importProjectSubmit"),
            "Import Project")
        )
      ),
      easyClose = TRUE
    )
  }
  
  # load project modal
  showLoadProject <- function(failed = c()) {
    dbRes <- globalManagers$projectManager()$projectTable()
    
    # make column names more readable
    colnames(dbRes) <- c(
      "Project", "Name", "Type", "Date/Time")
    
    modalDialog(
      if (nrow(dbRes) == 0) {
        "Create a project"
      } else {
        fluidRow(column(
          12,
          DT::renderDataTable({
            datatable(
              cbind(
                " " = shinyInput(
                  "actionButton", session$ns("selectedProject_"), dbRes$Project,
                  initLabel = rep(btnLABEL_SELECT, nrow(dbRes)),
                  initOnclick = paste(
                    sprintf(
                      'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
                      session$ns("loadSelectedProject"),
                      dbRes$Project
                    )
                  )
                ),
                " " = shinyInput(
                  "actionButton", session$ns("deletedProject_"), dbRes$Project,
                  initLabel = rep(btnLABEL_DELETE, nrow(dbRes)), class = btnCLASS_IMPORTANT,
                  initOnclick = paste(
                    sprintf(
                      'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
                      session$ns("deleteSelectedProject"),
                      dbRes$Project
                    )
                  )
                ),
                dbRes
              ),
              options = list(
                dom = 'tp', ordering = FALSE, lengthChange = FALSE,
                scrollX = TRUE, scrollY = FALSE, scrollCollapse = TRUE,
                autoWidth = FALSE
              ),
              rownames = FALSE, escape = FALSE, selection = 'none',
              editable = FALSE,
              style = "bootstrap4"
            ) %>% formatStyle(columns = colnames(dbRes))
            # backgroundColor = "#222")
          }),
        ))
      },
      easyClose = TRUE
    )
  }
  
  # load version modal
  showLoadVersion <- function(failed = c()) {
    dbRes <- globalManagers$projectManager()$projectVersionTable()
    
    # make column names more readable
    colnames(dbRes) <- c(
      "Version", "Date/Time", "Shiny bookmark", "Comment")
    
    modalDialog(
      fluidRow(column(
        12,
        if ("deleteSelectedVersion" %in% failed) {
          tags$div(errDELETE_VERSION_SELF,
                   class = errCLASS)
        },
        DT::renderDataTable({
          datatable(
            cbind(
              " " = shinyInput(
                "actionButton", session$ns("selectedVersion_"), dbRes$Version,
                initLabel = rep(btnLABEL_SELECT, nrow(dbRes)),
                initOnclick = paste(
                  sprintf(
                    'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
                    session$ns("loadSelectedVersion"),
                    dbRes$Version
                  )
                )
              ),
              " " = shinyInput(
                "actionButton", session$ns("deletedVersion_"), dbRes$Version,
                initLabel = rep(btnLABEL_DELETE, nrow(dbRes)), class = btnCLASS_IMPORTANT,
                initOnclick = paste(
                  sprintf(
                    'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
                    session$ns("deleteSelectedVersion"),
                    dbRes$Version
                  )
                )
              ),
              dbRes
            ),
            options = list(
              dom = 'tp', ordering = FALSE, lengthChange = FALSE,
              scrollX = TRUE, scrollY = FALSE, scrollCollapse = TRUE,
              autoWidth = FALSE
            ),
            rownames = FALSE, escape = FALSE, selection = 'none',
            editable = FALSE,
            style = "bootstrap4"
          ) %>%
            formatStyle(
              columns = colnames(dbRes))
              # backgroundColor = "#222")
        }),
      )),
      easyClose = TRUE
    )
  }
  
  # save managers in state
  saveManagerValuesInState <- function(state, managers){
    for (curManagerName in names(managers)) {
      curManager <- managers[[curManagerName]]()
      
      # add settings
      managerFunctions <- names(curManager)
      
      # get all getters
      managerFunctions <- managerFunctions[startsWith(managerFunctions, "get")]
      state$values[[curManagerName]] <- list()
      
      # save manager in state
      for (i in managerFunctions) {
        curFunctionName <- stringr::str_replace(i, "get", "")
        
        # call manager function
        state$values[[curManagerName]][[curFunctionName]] <- curManager[[i]]()
      }
    }
    
    return(state)
  }
  
  # load managers from state
  loadManagerValuesFromState <- function(state, manager, stateName) {
    # restore manager values
    for (i in names(state$values[[stateName]])) {
      manager()[[paste0("set", i)]](
        state$values[[stateName]][[i]])
    }
    
    return(manager)
  }
  
  ### Bookmarking
  
  # exclude items
  setBookmarkExclude(SHINY_BOOKMARK_EXCLUDE)
  
  # save extra values in state$values before bookmark
  onBookmark(function(state) {
    # add version and time information
    state$values$CCIAVERSION <- CCIAVERSION
    
    # get current time
    timeNow = as.character(Sys.time())
    globalManagers$projectManager()$setProjectVersionTp(timeNow)
    
    # set version
    if (is.null(globalManagers$projectManager()$getProjectVersionID())){
      globalManagers$projectManager()$setProjectVersionID(1)
    }
    
    # save manager values
    state <- saveManagerValuesInState(
      state,
      list(
        "project" = globalManagers$projectManager,
        "data" = globalManagers$dataManager,
        "viewer" = globalManagers$viewerManager
        )
    )
    
    # save collections
    globalManagers$dataManager()$saveCciaCollections()
  })
  
  # after bookmarked has happened
  # the purpose of this function is
  # handle the URL that is generated
  onBookmarked(function(url) {
    req(globalManagers$projectManager()$getProjectPath())
    
    # extract state id
    stateID <- stringr::str_match(url, "(?<=_state_id_\\=).*")
    
    # save in settings
    globalManagers$projectManager()$setProjectVersionStateID(stateID)
    
    # update project table
    globalManagers$projectManager()$updateProjectTable()
    
    # update version table
    globalManagers$projectManager()$updateProjectVersionTable(stateID)
  })
  
  # read values from state$values before restore
  onRestore(function(state) {
    # check whether the versions are the same
    # otherwise it might not work
    # if (state$values$CCIAVERSION != CCIAVERSION){
    #   showNotification(sprintf(
    #     "Version mismatch (%s - %s)",
    #     state$values$CCIAVERSION, CCIAVERSION))
    # } else {
    # load managers
    globalManagers$projectManager <<- loadManagerValuesFromState(
      state, globalManagers$projectManager, "project")
    globalManagers$dataManager <<- loadManagerValuesFromState(
      state, globalManagers$dataManager, "data")
    globalManagers$viewerManager <<- loadManagerValuesFromState(
      state, globalManagers$viewerManager, "viewer")
    # }
  })
  
  # after restored has happened
  onRestored(function(state) {
    globalManagers$projectManager()$updateStateID()
    
    # show import images tab
    # by default, this will be import images
    tabImportImages <- paste0(
      globalManagers$projectManager()$getProjectType(),
      "ImportImages")
    
    showTab(
      inputId = "sidebar",
      target = tabImportImages,
      select = TRUE)
    
    showNotification(sprintf(
      'Version %d restored from %s',
      globalManagers$projectManager()$getProjectVersionID(),
      globalManagers$projectManager()$getProjectVersionTp()
    ))
  })

  ### Reactive values
  
  ### Reactive-like values
  
  # listen to viewer input
  viewerOuput <- reactivePoll(
    cciaConf()$python$viewer$outputDelay, session,
    checkFunc = function() {
      outputFile <- file.path(
        cciaPath(), "app",
        cciaConf()$python$viewer$viewerPath,
        cciaConf()$python$viewer$outputFile
      )
      
      if (file.exists(outputFile)) {
        return(file.mtime(outputFile))
      } else {
        return("")
      }
    },
    valueFunc = function() {
      outputFile <- file.path(
        cciaPath(), "app",
        cciaConf()$python$viewer$viewerPath,
        cciaConf()$python$viewer$outputFile
      )
      
      retVal <- NULL
      
      if (file.exists(outputFile)) {
        if (file.info(outputFile)$size > 1) {
          retVal <- jsonlite::fromJSON(outputFile)
        }
      }
      
      retVal
    }
  )
  
  ### Reactives - RxCalc
  ## Event specific
  
  ## Generic
  
  ### Observers - RxAction
  ## Event specific
  
  # Viewer options
  observeEvent(input$viewerParams, {
    # split channels?
    # if ("useChannelAxis" %in% input$viewerParams) {
    globalManagers$viewerManager()$setUseChannelAxis(TRUE)
    # } else {
    #   globalManagers$viewerManager()$setUseChannelAxis(FALSE)
    # }
    
    # show viewer?
    # if ("showViewer" %in% input$viewerParams) {
    #   globalManagers$viewerManager()$setShowViewer(TRUE)
    # } else {
    #   globalManagers$viewerManager()$setShowViewer(FALSE)
    # }
    
    # show original?
    if ("showOriginal" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShowOriginal(TRUE)
    } else {
      globalManagers$viewerManager()$setShowOriginal(FALSE)
    }
    
    # visible layers?
    if ("layersVisible" %in% input$viewerParams) {
      globalManagers$viewerManager()$setLayersVisible(TRUE)
    } else {
      globalManagers$viewerManager()$setLayersVisible(FALSE)
    }
    
    # show 3D?
    if ("show3D" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShow3D(TRUE)
    } else {
      globalManagers$viewerManager()$setShow3D(FALSE)
    }
    
    # show labels?
    if ("showLabels" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShowLabels(TRUE)
    } else {
      globalManagers$viewerManager()$setShowLabels(FALSE)
    }
    
    # show points?
    if ("showPoints" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShowPoints(TRUE)
    } else {
      globalManagers$viewerManager()$setShowPoints(FALSE)
    }
    
    # show tracks?
    if ("showTracks" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShowTracks(TRUE)
    } else {
      globalManagers$viewerManager()$setShowTracks(FALSE)
    }
    
    # load image as dask - ie/ delayed loading?
    if ("asDask" %in% input$viewerParams) {
      globalManagers$viewerManager()$setAsDask(TRUE)
    } else {
      globalManagers$viewerManager()$setAsDask(FALSE)
    }
    
    # squeeze image?
    if ("squeeze" %in% input$viewerParams) {
      globalManagers$viewerManager()$setSqueezeImage(TRUE)
    } else {
      globalManagers$viewerManager()$setSqueezeImage(FALSE)
    }
    
    # Downsample Z?
    if ("downsampleZ" %in% input$viewerParams) {
      globalManagers$viewerManager()$setDownsampleZ(TRUE)
    } else {
      globalManagers$viewerManager()$setDownsampleZ(FALSE)
    }
    
    # Repload image?
    if ("reloadImage" %in% input$viewerParams) {
      globalManagers$viewerManager()$setReloadImage(TRUE)
    } else {
      globalManagers$viewerManager()$setReloadImage(FALSE)
    }
    
    # show populations?
    if ("showPops" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShowPops(TRUE)
    } else {
      globalManagers$viewerManager()$setShowPops(FALSE)
    }
    
    # show neighbours?
    if ("showNeighbours" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShowNeighbours(TRUE)
    } else {
      globalManagers$viewerManager()$setShowNeighbours(FALSE)
    }
    
    # show branching?
    if ("showBranching" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShowBranching(TRUE)
    } else {
      globalManagers$viewerManager()$setShowBranching(FALSE)
    }
    
    # show shapes?
    if ("showShapes" %in% input$viewerParams) {
      globalManagers$viewerManager()$setShowShapes(TRUE)
    } else {
      globalManagers$viewerManager()$setShowShapes(FALSE)
    }
  })
  
  # Viewer branching property
  observeEvent(input$viewerBranchingProperty, {
    req(input$viewerBranchingProperty)
    
    globalManagers$viewerManager()$setBranchingProperty(input$viewerBranchingProperty)
  })
  
  # Viewer multiscales
  observeEvent(input$viewerMultiscales, {
    req(input$viewerMultiscales)
    
    # how many multiscales?
    globalManagers$viewerManager()$setMultiscales(input$viewerMultiscales)
  })
  
  # close Viewer
  observeEvent(input$closeViewer, {
    globalManagers$viewerManager()$closeViewer()
  })
  
  # add animation pane to Viewer
  observeEvent(input$viewerAddAnimationPane, {
    globalManagers$viewerManager()$addAnimationPane()
  })
  
  # update viewer input
  observeEvent(viewerOuput(), {
    globalManagers$viewerManager()$updateOutput(viewerOuput())
  })
    
  # enable buttons once a project is loaded
  observeEvent(globalManagers$projectManager()$getProjectName(), {
    req(globalManagers$projectManager()$getProjectName())
    
    enable("saveProject")
    enable("exportProject")
    enable("loadVersion")
    enable("createVersion")
  })
  
  # save project
  observeEvent(input$saveProject, {
    req(globalManagers$projectManager()$getProjectPath())
    
    globalManagers$projectManager()$doProjectBookmark()
    
    # show message
    showNotification(
      msgPROJECT_SAVED[1],
      type = msgPROJECT_SAVED[2])
  })
  
  # export project
  observeEvent(input$exportProject, {
    showModal(showExportProject())
  })
  
  # load project
  observeEvent(input$loadProject, {
    showModal(showLoadProject())
  })
  
  # import project
  observeEvent(input$importProject, {
    showModal(showImportProject())
  })
  
  # load selected project
  observeEvent(input$loadSelectedProject, {
    globalManagers$projectManager()$setProjectUID(
      input$loadSelectedProject
    )

    # load project version
    globalManagers$projectManager()$loadProjectVersion()
  })
  
  # delete selected version
  observeEvent(input$deleteSelectedProject, {
    progress <- Progress$new()
    
    if (globalManagers$projectManager()$useHPC() == TRUE) {
      progress$set(message = "Delete HPC files ... ", value = 0)
      globalManagers$projectManager()$deleteHPCProjectContent(input$deleteSelectedProject)
    }
    
    progress$set(message = "Delete local files ... ", value = 50)
    globalManagers$projectManager()$deleteProject(input$deleteSelectedProject)
    
    progress$close()
    
    showModal(showLoadProject())
  })
  
  # load version
  observeEvent(input$loadVersion, {
    showModal(showLoadVersion())
  })
  
  # load selected version
  observeEvent(input$loadSelectedVersion, {
    globalManagers$projectManager()$loadProjectVersion(
      as.numeric(input$loadSelectedVersion)
    )
  })
  
  # delete selected version
  observeEvent(input$deleteSelectedVersion, {
    failedInputs <- c()
    
    curSelVersion <- as.numeric(
      input$deleteSelectedVersion)
    
    # can only delete versions that are not selected
    if (curSelVersion ==
        globalManagers$projectManager()$getProjectVersionID()) {
      failedInputs <- c(
        failedInputs, "deleteSelectedVersion")
    }
    
    if (length(failedInputs) > 0){
      # refresh modal
      showModal(showLoadVersion(failedInputs))
    } else {
      progress <- Progress$new()
      
      if (globalManagers$projectManager()$useHPC() == TRUE) {
        progress$set(message = "Delete HPC files ... ", value = 0)
        globalManagers$projectManager()$deleteHPCVersionContent(curSelVersion)
      }
      
      progress$set(message = "Delete local files ... ", value = 50)
      globalManagers$projectManager()$deleteProjectVersion(curSelVersion)
      
      progress$close()
      
      showModal(showLoadVersion())
    }
  })
  
  # show create version
  observeEvent(input$createVersion, {
    showModal(showCreateVersion())
  })
  
  # create a new version
  observeEvent(input$createVersionSubmit, {
    # go through inputs
    failedInputs <- c()
    
    if (input$createVersionComment == "") {
      failedInputs <- c(
        failedInputs, "createVersionComment")
    }
    
    # create project version
    if (length(failedInputs) > 0) {
      showModal(showCreateVersion(failedInputs))
    } else {
      newProjectVersion <- globalManagers$projectManager()$createProjectVersion()
      
      progress <- Progress$new()
      progress$set(message = "Copy local files ... ", value = 0)
      
      # copy data from current to new version
      # local
      globalManagers$projectManager()$copyVersionContent(
        globalManagers$projectManager()$getProjectVersionID(), 
        newProjectVersion)
      
      if (globalManagers$projectManager()$useHPC() == TRUE) {
        progress$set(message = "Copy HPC files ... ", value = 40)
        
        # on HPC
        globalManagers$projectManager()$copyHPCVersionContent(
          globalManagers$projectManager()$getProjectVersionID(), 
          newProjectVersion)
      }
      
      # set settings
      globalManagers$projectManager()$setProjectVersionComment(input$createVersionComment)
      globalManagers$projectManager()$setProjectVersionID(newProjectVersion)
      
      progress$set(message = "Create bookmark ... ", value = 90)
      
      # create a bookmark
      globalManagers$projectManager()$doProjectBookmark(FALSE)
      
      progress$set(message = "Files copied", value = 100)
      progress$close()
      
      # show message
      showNotification(
        msgPROJECT_VERSION_CREATED[1],
        type = msgPROJECT_VERSION_CREATED[2])
    }
  })
  
  # export project
  observeEvent(input$exportProjectSubmit, {
    # go through inputs
    failedInputs <- c()
    
    exportPath <- joinSelectedPath(
      input$exportProjectPath, useConfigVolumes = TRUE)
    
    # if (input$exportProjectPath == "") {
    #   failedInputs <- c(
    #     failedInputs, "exportProjectPath")
    # }
    
    # export project
    if (length(failedInputs) > 0) {
      showModal(showExportProject(failedInputs))
    } else {
      progress <- Progress$new()
      progress$set(message = "Export project ... ", value = 0)
      
      globalManagers$projectManager()$exportProject(exportPath)
      
      progress$set(message = "Project exported", value = 100)
      progress$close()  
      
      # show message
      showNotification(
        msgPROJECT_EXPORTED[1],
        type = msgPROJECT_EXPORTED[2])
    }
  })
  
  # import project
  observeEvent(input$importProjectSubmit, {
    # go through inputs
    failedInputs <- c()
    
    # check that a file was selected
    if (!any("files" %in% names(input$importProjectPath))) {
      failedInputs <- c(
        failedInputs, "importProjectPath")
    }
    
    importPath <- joinSelectedFile(input$importProjectPath)
    
    # import project
    if (length(failedInputs) > 0) {
      showModal(showImportProject(failedInputs))
    } else {
      progress <- Progress$new()
      progress$set(message = "Import project ... ", value = 0)
      
      globalManagers$projectManager()$importProject(importPath)
      
      progress$set(message = "Project imported", value = 100)
      progress$close()  
      
      # show message
      showNotification(
        msgPROJECT_IMPORTED[1],
        type = msgPROJECT_IMPORTED[2])
    }
  })
  
  # shutdown
  # https://stackoverflow.com/a/43864498/13766165
  observeEvent(input$shutdown, {
    # close viewer
    # globalManagers$viewerManager()$closeViewer()
    
    # shutdown python kernel
    if (DEBUG_NO_VIEWER_SHUTDOWN == FALSE) {
      globalManagers$viewerManager()$quit()
    }
    
    stopApp()
  })
  
  ## Generic
  
  ### UI Outputs
  
  ## Text
  # project name
  output$projectName <- renderUI({
    req(globalManagers$projectManager()$getProjectName())
    
    HTML(paste0(
      globalManagers$projectManager()$getProjectName(),
      tags$br(),
      "v", globalManagers$projectManager()$getProjectVersionID()
    ))
  })
  
  ## Tables
  
  ## Plots
  
  ## Buttons
  
  ## Menu items
  # create project
  output$menuCreateProject <- renderMenu({
    req(is.null(globalManagers$projectManager()$getProjectType()))
    
    menuItem(
      "Create Project",
      icon = icon("paw"),
      tabName = "createProject")
  })
  
  # analysis
  output$menuStaticAnalysis <- renderMenu({
    req(globalManagers$projectManager()$getProjectType() == "static")
    
    # show menu items
    menuItem(
      "Static Analysis",
      startExpanded = TRUE,
      selected = TRUE,
      icon = icon("person-dress"),
      tabName = "staticAnalysis",
      menuSubItem(
        "Import Images",
        selected = TRUE,
        icon = icon("list-ul"),
        tabName = "importImages"),
      menuSubItem(
        "Image Metadata",
        icon = icon("carrot"),
        tabName = "manageMetadata"),
      menuSubItem(
        "Cleanup images",
        icon = icon("cloud-sun"),
        tabName = "cleanupImages"),
      menuSubItem(
        "Train models",
        icon = icon("paintbrush"),
        tabName = "trainModels"),
      menuSubItem(
        "Cell Segmentation",
        icon = icon("splotch"),
        tabName = "segmentImages"),
      menuSubItem(
        "Population Gating",
        icon = icon("draw-polygon"),
        tabName = "gatePopulations"),
      menuSubItem(
        "Population Clustering",
        icon = icon("map"),
        tabName = "clustPopulations"),
      menuSubItem(
        "Region Clustering",
        icon = icon("globe"),
        tabName = "clustRegions"),
      menuSubItem(
        "Pixel Classification",
        icon = icon("robot"),
        tabName = "pixelClassification"),
      menuSubItem(
        "Spatial Analysis",
        icon = icon("chart-pie"),
        tabName = "spatialAnalysis"),
      menuSubItem(
        "Signal Analysis",
        icon = icon("bolt"),
        tabName = "signalAnalysis")
    )
  })
  
  output$menuLiveAnalysis <- renderMenu({
    req(globalManagers$projectManager()$getProjectType() == "live")
    
    menuItem(
      "Live Analysis",
      startExpanded = TRUE,
      selected = TRUE,
      icon = icon("person-running"),
      tabName = "liveAnalysis",
      menuSubItem(
        "Import Images",
        selected = TRUE,
        icon = icon("list-ul"),
        tabName = "importImages"),
      menuSubItem(
        "Image Metadata",
        icon = icon("carrot"),
        tabName = "manageMetadata"),
      menuSubItem(
        "Cleanup images",
        icon = icon("cloud-sun"),
        tabName = "cleanupImages"),
      menuSubItem(
        "Train models",
        icon = icon("paintbrush"),
        tabName = "trainModels"),
      menuSubItem(
        "Cell Segmentation",
        icon = icon("splotch"),
        tabName = "segmentImages"),
      menuSubItem(
        "Cell Tracking",
        icon = icon("hat-wizard"),
        tabName = "trackingImages"),
      menuSubItem(
        "Cell Behaviour",
        icon = icon("rocket"),
        tabName = "behaviourAnalysis"),
      menuSubItem(
        "Population Clustering",
        icon = icon("map"),
        tabName = "clustPopulations"),
      # menuSubItem(
      #   "Cell Mapping",
      #   icon = icon("map"),
      #   tabName = "liveMapping"),
      menuSubItem(
        "Spatial analysis",
        icon = icon("chart-pie"),
        tabName = "spatialAnalysis"),
      menuSubItem(
        "Signal analysis",
        icon = icon("bolt"),
        tabName = "signalAnalysis")
    )
  })
  
  output$menuFlowAnalysis <- renderMenu({
    req(globalManagers$projectManager()$getProjectType() == "flow")
    
    menuItem(
      "Flow Cytometry",
      startExpanded = TRUE,
      selected = TRUE,
      icon = icon("vector-square"),
      tabName = "flowAnalysis",
      menuSubItem(
        "Import Flow Files",
        selected = TRUE,
        icon = icon("list-ul"),
        tabName = "importFlow"),
      menuSubItem(
        "Image Metadata",
        icon = icon("carrot"),
        tabName = "manageMetadata"),
      menuSubItem(
        "Population Gating",
        icon = icon("draw-polygon"),
        tabName = "gatePopulations"),
      menuSubItem(
        "Population Clustering",
        icon = icon("map"),
        tabName = "clustPopulations")
    )
  })
  
  output$menuPlotCanvas <- renderMenu({
    menuItem(
      "Plot canvas",
      startExpanded = TRUE,
      selected = TRUE,
      icon = icon("clover"),
      tabName = "plotCanvas",
      menuSubItem(
        "Population plots",
        selected = TRUE,
        icon = icon("chart-simple"),
        tabName = "plotCharts"),
      menuSubItem(
        "Interaction heatmaps",
        icon = icon("circle-nodes"),
        tabName = "plotInteractionHeatmaps"),
      menuSubItem(
        "Cluster heatmaps",
        icon = icon("fire"),
        tabName = "plotHeatmaps"),
      menuSubItem(
        "Cluster UMAPs",
        icon = icon("signs-post"),
        tabName = "plotClustersUMAP"),
      menuSubItem(
        "Flow Gating",
        icon = icon("paintbrush"),
        tabName = "plotFlowGating"),
      menuSubItem(
        "Population densities",
        icon = icon("basketball"),
        tabName = "plotPopDensities")
      # menuSubItem(
      #   "Tracking Cluster UMAPs",
      #   icon = icon("shuffle"),
      #   tabName = "plotTrackClustersUMAP")
    )
  })
  
  # settings
  output$menuSettings <- renderMenu({
    req(globalManagers$projectManager()$getProjectType())
    
    menuItem(
      "Settings",
      icon = icon("gears"),
      tabName = "projectSettings")
  })
  
  ## Other
  shinyDirChoose(
    input, "exportProjectPath",
    roots = cciaConf()$volumes,
    session = session)
  
  shinyFileChoose(
    input, "importProjectPath",
    roots = shinyFiles::getVolumes(),
    session = session,
    hidden = FALSE,
    filetypes = cciaConf()$files$exportTypes)
  
  ### Call Modules
  # create/load project
  .createProjectServer("init", session, globalManagers)
  
  # general analysis
  .importImagesServer("importImages", session, globalManagers)
  .importFlowServer("importFlow", session, globalManagers)
  .manageMetadataServer("metadata", session, globalManagers)
  .segmentImagesServer("segment", session, globalManagers)
  
  # specific to static images?
  .clustPopulationsServer("clustPopulations", session, globalManagers)
  .clustRegionsServer("clustRegions", session, globalManagers)
  .gatePopulationsServer("gatePopulations", session, globalManagers)
  
  # plot canvases
  .plotChartsServer("plotCharts", session, globalManagers)
  .plotHeatmapsServer("plotHeatmaps", session, globalManagers)
  .plotInteractionHeatmapsServer("plotInteractionHeatmaps", session, globalManagers)
  .plotClustersUMAPServer("plotClustersUMAP", session, globalManagers)
  .plotFlowGatingServer("plotFlowGating", session, globalManagers)
  .plotPopDensitiesServer("plotPopDensities", session, globalManagers)
  
  # specific to live?
  .cleanupImagesServer("cleanupImages", session, globalManagers)
  .trainModelsServer("trainModels", session, globalManagers)
  .trackingImagesServer("tracking", session, globalManagers)
  
  .pixelClassificationServer("pixcl", session, globalManagers)
  .spatialAnalysisServer("spatialAnalysis", session, globalManagers)
  .signalAnalysisServer("signalAnalysis", session, globalManagers)
  .behaviourAnalysisServer("behaviourAnalysis", session, globalManagers)
  
  # settings
  .projectSettingsServer("global", session, globalManagers)
}
