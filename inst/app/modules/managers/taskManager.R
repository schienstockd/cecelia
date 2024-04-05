# Task Manager
createTaskManager <- function(
  input, output, session, globalManagers, moduleManagers, managerConf) {
  # variables
  if (!"runTaskCombined" %in% names(managerConf$task)) {
    managerConf$task$runTaskCombined <- FALSE
  }
  if (!"runTaskCombinedSelect" %in% names(managerConf$task)) {
    managerConf$task$runTaskCombinedSelect <- FALSE
  }
  
  # set buttons to enable selection from UI
  if (managerConf$task$runTaskCombinedSelect == TRUE)
    enable("runTaskCombined")
  
  ### Reactive values
  # currently shown object for logs
  taskLogObj <- reactiveVal()
  taskLogObservers <- list()
  taskLogListeners <- list()
  taskLogContent <- list()
  
  # task states
  curTaskStates <- reactiveVal()
  prevTaskStates <- reactiveVal(list())
  
  # task UI
  funParamsUI <- reactiveVal()
  # funParamsReactives <- list()
  
  ### Reactive-like values
  tasksCounter <- list()
  taskMonitorUtils <- TaskMonitorUtils$new()$reactive()
  
  makeReactiveBinding("tasksCounter")
  
  ### Reactives - RxCalc
  ## Event specific
  
  ## Generic
  
  # change task states
  changedTaskStates <- eventReactive(curTaskStates(), {
    req(curTaskStates())
    
    # compare with previous states
    if (length(prevTaskStates()) > 0) {
      mapply(
        function(x, i) {
          x[!x %in% prevTaskStates()[[i]]]
        }, curTaskStates(), names(curTaskStates()), SIMPLIFY = FALSE
      )
    } else {
      curTaskStates()
    }
  }) %>% debounce(cciaConf()$tasks$results$poll)
  # }) %>% throttle(cciaConf()$tasks$results$poll)
  
  # get environments of current function
  taskFunctionSettings <- reactive({
    req(taskFunction())
    
    moduleManagers()$inputManager$funNames()[[
      .trimModuleFunName(taskFunction())]]
  })
  
  # supply how many tasks this function allows
  # TODO at the moment, if tasks access the same
  # cciaObj, modify and change it
  # the tasks will collide .. have to implement
  # some kind of locking mechanism for RDS
  taskFunctionTasksLimit <- reactive({
    req(taskFunctionSettings())
    
    retVal <- cciaConf()$tasks$tasksLimit
    
    # check whether the function has a limit
    if ("tasksLimit" %in% names(taskFunctionSettings())) {
      retVal <- taskFunctionSettings()$tasksLimit
    }
    
    retVal
  })
  
  # supply where the function can run
  taskFunctionEnvironments <- reactive({
    req(taskFunctionSettings())
    
    # get environments of current function
    funEnv <- taskFunctionSettings()$env
    
    # check whether HPC can be used
    if ("hpc" %in% names(funEnv)) {
      if (!globalManagers$projectManager()$useHPC()) {
        funEnv <- funEnv[funEnv != 'hpc']
      }
    }
    
    funEnv
  })
  
  # task function list
  taskFunctionList <- reactive({
    # get functions
    funNames <- lapply(moduleManagers()$inputManager$funNames(), function(x) x$label)
    funCategories <- lapply(moduleManagers()$inputManager$funNames(), function(x) x$category)
    
    sortedFunSelection <- list()
    
    if (length(funNames) > 0) {
      # add module name
      names(funNames) <- paste(
        managerConf$moduleName, names(funNames), sep = cecelia:::CCID_CLASS_SEP)
      
      # switch labels to names
      moduleFunctions <- .reverseNamedList(funNames)
      
      # push functions into categories
      funSelection <- list()
      
      for (i in seq(length(moduleFunctions))) {
        curCat <- funCategories[[i]]
        
        # create list of functions
        if (!(curCat %in% names(funSelection))) {
          funSelection[[curCat]] <- list()
        }
        
        funSelection[[curCat]] <- append(
          funSelection[[curCat]],
          moduleFunctions[i]
        )
      }
      
      # sort by categories
      catNames <- names(funSelection)
      sortedCats <- sort(catNames)
      
      # sort function selection
      for (x in sortedCats) {
        sortedFunSelection[[x]] <- funSelection[[x]]
      }
    }
    
    sortedFunSelection
  })
  
  # task function name
  taskFunction <- reactive({
    req(input$taskFunction)
    
    input$taskFunction
  })
  
  # logfile names
  taskLogfilepaths <- eventReactive(c(
    taskMonitorUtils(), 
    input$showLog,
    taskLogObj()
  ), {
    req(input$showLog)
    req(taskLogObj())
    req(length(taskMonitorTaskList()) > 0)
    req(input$showLog %in% names(taskMonitorTaskList()))
    req(!input$showLog %in% names(taskMonitorUtils()$queuedTasks()))
    
    logfiles <- taskMonitorUtils()$getTask(input$showLog)$logfilepaths()
    
    # add names
    names(logfiles) <- sapply(logfiles, basename)
    
    logfiles
  })
  
  # task list
  taskMonitorTaskList <- reactive({
    taskMonitorUtils()$getTaskList()
  })
  
  # Task DT
  taskMonitorTaskDT <- reactive({
    getContainerAttributes(rev(taskMonitorTaskList()), asDT = TRUE)
  })
  
  # task combined rather than individual images
  runTaskCombined <- reactive({
    # get value from input
    if (managerConf$task$runTaskCombinedSelect == TRUE) {
      input$runTaskCombined
    } else {
      # otherwise get from configuration from manager config
      managerConf$task$runTaskCombined
    }
  })
  
  # task on set rather than individual jobs
  runTaskOnSet <- reactive({
    # get value from input
    input$runTaskOnSet
  })
  
  # upload local cciaObj
  uploadCciaObj <- reactive({
    # get value from input
    input$uploadCciaObj
  })
  
  # environment selection
  taskEnvironment <- reactive({
    req(input$taskEnvironment)
    
    input$taskEnvironment
  })
  
  # add task vars
  taskVarsToAdd <- reactive({
    retVal <- NULL
    
    if ("taskVarsToAdd" %in% names(managerConf$task)) {
      retVal <- managerConf$task$taskVarsToAdd()
    }
    
    retVal
  })
  
  # task limit
  taskLimit <- reactive({
    input$taskLimit
  }) %>% debounce(cciaConf()$tasks$results$poll)
  
  # number of tasks
  numRunningTasks <- reactive({
    length(taskMonitorUtils()$runningTasks())
  })
  
  numQueuedTasks <- reactive({
    length(taskMonitorUtils()$queuedTasks())
  })
  
  numTotalBusyTasks <- reactive({
    numRunningTasks() + numQueuedTasks()
  })
  
  numSuccessTasks <- reactive({
    length(taskMonitorUtils()$successTasks())
  })
  
  numFailedTasks <- reactive({
    length(taskMonitorUtils()$failedTasks())
  })
  
  # HPC job parameters
  taskHPCnumNodes <- reactive({
    req(input$taskHPCnumNodes)
    
    retValue <- as.numeric(input$taskHPCnumNodes)
    
    if (!checkForNumbersOnly(input$taskHPCnumNodes)) {
      retValue <- NULL
    } 
    
    retValue
  })
  
  taskHPCnumTasks <- reactive({
    req(input$taskHPCnumTasks)
    
    retValue <- as.numeric(input$taskHPCnumTasks)
    
    if (!checkForNumbersOnly(input$taskHPCnumTasks)) {
      retValue <- NULL
    } 
    
    retValue
  })
  
  taskHPCnumCPUperTask <- reactive({
    req(input$taskHPCnumCPUperTask)
    
    retValue <- as.numeric(input$taskHPCnumCPUperTask)
    
    if (!checkForNumbersOnly(input$taskHPCnumCPUperTask)) {
      retValue <- NULL
    } 
    
    retValue
  })
  
  taskHPCnumGPUperTask <- reactive({
    req(input$taskHPCnumGPUperTask)
    
    retValue <- as.numeric(input$taskHPCnumGPUperTask)
    
    if (!checkForNumbersOnly(input$taskHPCnumGPUperTask)) {
      retValue <- NULL
    } 
    
    retValue
  })
  
  taskHPCmemory <- reactive({
    req(input$taskHPCmemory)
    
    retValue <- as.numeric(input$taskHPCmemory)
    
    if (!checkForNumbersOnly(input$taskHPCmemory)) {
      retValue <- NULL
    } 
    
    retValue
  })

  taskHPCwalltime <- reactive({
    req(input$taskHPCwalltime)
    
    retValue <- input$taskHPCwalltime
    
    if (any(is.na(stringr::str_match(
      input$taskHPCwalltime,
      "^[0-9]{2}\\-[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}$"))) == TRUE) {
      retValue <- NULL
    } 
    
    retValue
  })

  # return HPC project ID
  hpcProjectID <- reactive({
    projectID <- NULL
    
    if (useGPU() == TRUE) {
      projectID <- globalManagers$projectManager()$getProjectHPCprojectGPU()
    } else {
      projectID <- globalManagers$projectManager()$getProjectHPCprojectCPU()
    }
    
    paste0(cciaConf()$hpc$dirs$projectPrefix, projectID)
  })
  
  # return HPC partitions
  hpcProjectPartitions <- reactive({
    partitions <- NULL
    
    if (useGPU() == TRUE) {
      partitions <- globalManagers$projectManager()$getProjectHPCpartitionsGPU()
    } else {
      partitions <- globalManagers$projectManager()$getProjectHPCpartitionsCPU()
    }
    
    partitions
  })
  
  # return HPC qos
  hpcProjectQos <- reactive({
    qos <- NULL
    
    if (useGPU() == TRUE) {
      qos <- globalManagers$projectManager()$getProjectHPCqosGPU()
    } else {
      qos <- globalManagers$projectManager()$getProjectHPCqosCPU()
    }
    
    qos
  })
  
  useGPU <- reactive({
    if (useHPC() == TRUE)
      input$taskHPCuseGPU
    else
      input$taskLocalUseGPU
  })
  
  useMATLAB <- reactive({
    if (useHPC() == TRUE)
      input$taskHPCuseMATLAB
    else
      FALSE
  })
  
  # use HPC for task?
  useHPC <- reactive({
    taskEnvironment() == "hpc"
  })
  
  # sync with HPC?
  taskHPCsync <- reactive({
    input$taskHPCsync
  })
  
  # get selected or shown ccia object
  cciaObj <- reactive({
    cciaObj <- NULL
    
    selectedUIDs <- moduleManagers()$selectionManager$selectedUIDs()
    
    # first selected image
    if (length(selectedUIDs) > 0) {
      cciaObj <- firstSelectedImageFromSet(
        selectedUIDs,
        moduleManagers()$imageSetManager$selectedSet()
      )
    }
    
    # Get object by shown image
    if (is.null(cciaObj)) {
      if ("imageViewerManager" %in% names(moduleManagers())) {
        cciaObj <- moduleManagers()$imageViewerManager$shownImage()
      } 
    }
    
    cciaObj
  })
  
  # return selected functions
  taskMonitorShowSelectedFunction <- reactive({
    # add launchpad set
    c(
      "launchpad.runCciaSet",
      input$taskMonitorShowSelectedFunction
    )
  })
  
  # attributes for task monitor
  taskMonitorAttr <- reactive({
    # get container attributes
    containerAttr <- NULL
    
    # reverse that latest tasks are at the top
    if (length(taskMonitorTaskList()) > 0) {
      containerAttr <- taskMonitorTaskDT()
    
      # filter on container
      # uID
      if (length(input$taskMonitorUIDs) > 0) {
        containerAttr <- containerAttr[uID %in% input$taskMonitorUIDs]
      }
      
      # task functions
      if (length(input$taskMonitorTaskFunctions) > 0) {
        containerAttr <- containerAttr[taskFunction %in% input$taskMonitorTaskFunctions]
      }
      
      # selected task function
      if (input$taskMonitorShowSelectedFunction == TRUE)
        containerAttr <- containerAttr[taskFunction %in% c(
          "launchpad.runCciaSet", taskFunction()
          )]
      
      # latest task function
      if (input$taskMonitorShowLastTasks == TRUE) {
        # containerAttr <- containerAttr[taskID == max(taskID), by = .(uID, taskFunction)]
        containerAttr[, maxTaskID := max(taskID), by = .(uID, taskFunction)]
        containerAttr <- containerAttr[taskID == maxTaskID]
      }
    }
    
    containerAttr
  })
    # otherwise rendering the table is rendered
    # every time a task is added - this
    # happens often when submitting a lot
    # of tasks at the same time
    # debounce(500)
    # throttle(500)
  
  # output for task monitor
  taskMonitorTable <- reactive({
    # get table
    if (!is.null(taskMonitorAttr()) && nrow(taskMonitorAttr()) > 0)
      moduleManagers()$uiManager$dataTable(append(
        list(
          "uID" = taskMonitorAttr()$uID,
          "taskFunction" = taskMonitorAttr()$taskFunction,
          "taskID" = taskMonitorAttr()$taskID
          # "taskID" = containerNames
        ), createTaskDataTableColumns(taskMonitorAttr()$name)
      ), ordering = FALSE)
    else 
      moduleManagers()$uiManager$dataTable(
        list(
          " " = c("No Tasks to display")
        )
      )
  })
  
  ## Functions
  
  # delete task directory
  deleteTaskDirectory <- function(uID) {
    # get directory
    taskDir <- globalManagers$projectManager()$persistentObjectDirectory(uID)
    
    # remove
    unlink(taskDir, recursive = TRUE)
  }
  
  # prepare logfile paths
  prepLogfilepaths <- function(taskLauncher, logfilepaths, addTaskAttr = TRUE, removePrevious = TRUE) {
    # get local logfile
    taskLogName <- paste0(taskLauncher$getTaskFunction(), ".taskLog")
    systemLogName <- paste0(taskLauncher$getTaskFunction(), ".systemLog")
    
    logfilepaths[[taskLogName]] <- taskLauncher$taskLog(locationOnly = TRUE, local = TRUE)
    # will be handled by lanuchpad
    # logfilepaths[[systemLogName]] <- taskLauncher$taskOutputFile(local = TRUE)
    
    # add job paths
    # attr(logfilepaths[[taskLogName]], "remoteLog") <- taskLauncher$taskLog(locationOnly = TRUE, local = FALSE)
    # will be handled by lanuchpad
    # attr(logfilepaths[[systemLogName]], "remoteLog") <- taskLauncher$taskOutputFile(local = FALSE)
    
    # add task log if running on HPC
    if (useHPC() == TRUE) {
      # for local process
      attr(logfilepaths[[taskLogName]], "remoteLog") <- taskLauncher$taskLog(locationOnly = TRUE, local = FALSE)
      
      # for slurm submission
      logfilepaths[[systemLogName]] <- file.path(
        taskLauncher$envParams("local")$dirs$task,
        cciaConf()$dirs$tasks$tasks,
        paste(tools::file_path_sans_ext(basename(logfilepaths[[taskLogName]])), "slurm.log", sep = ".")
      )
      attr(logfilepaths[[systemLogName]], "remoteLog") <- paste(
        taskLauncher$envParams("hpc")$dirs$task,
        cciaConf()$dirs$tasks$tasks,
        paste(tools::file_path_sans_ext(basename(logfilepaths[[taskLogName]])), "slurm.log", sep = "."),
        sep = "/"
      )
    } 
    
    # add running environment and log
    if (addTaskAttr == TRUE) {
      attr(logfilepaths, "taskEnv") <- taskLauncher$taskEnv()
      attr(logfilepaths, "taskDir") <- taskLauncher$envParams()$dirs$task
    }
    
    # go through file paths
    if (removePrevious == TRUE) {
      for (y in logfilepaths) {
        # remove previous log
        if (file.exists(y)) {
          file.remove(y)
        }
      }
    }
    
    logfilepaths
  }
  
  # create a new task
  createNewTask <- function(uID, taskList = NULL, taskID = NULL, hpcExitOnly = NULL, taskFunction = NULL) {
    # remove from success or fail
    # every task will be logged
    # removeSuccessTask(uID)
    # removeFailedTask(uID)

    # get task function
    if (is.null(taskFunction)) {
      taskFunction <- taskFunction()
    }
    
    if (is.null(taskList)) {
      # prepare task variables
      taskVars <- taskLauncherVars(uID, taskEnvironment())
      
      # prepare parameters
      taskVars$fun <- moduleManagers()$inputManager$funParams(trim = TRUE)
      
      # are there any parameters for injection?
      if (!is.null(taskVarsToAdd())){
        taskVars$fun <- append(
          taskVars$fun, taskVarsToAdd()
        )
      }
      
      # add selected images as parameter?
      if (runTaskCombined() == TRUE) {
        taskVars$fun$uIDs <- unlist(moduleManagers()$selectionManager$selectedUIDs())
      }
      
      # watch exit only for HPC?
      if (!is.null(hpcExitOnly)) {
        taskVars$env$hpc$conf$exitOnly <- hpcExitOnly
      }
      
      # save function parameters for image set
      # ie/ the last used parameters will be saved
      # in the set
      if ("imageSetManager" %in% names(moduleManagers())) {
        moduleManagers()$imageSetManager$selectedSet()$saveModuleFunParams(
          taskFunction, taskVars$fun, invalidate = FALSE
        )
      }
      
      # get task ID
      if (is.null(taskID)) {
        taskID <- generateTaskID(uID, taskFunction)
      }
      
      # prepare launch list
      # TODO is this ok .. ?
      launchList <- list(
        envVars = taskVars$env,
        # hpcDir = if (useHPC() == TRUE) dirname(dirname(taskVars$env$hpc$dirs$task)) else NULL,
        hpcDir = if ("hpc" %in% names(taskVars$env)) dirname(dirname(taskVars$env$hpc$dirs$task)) else NULL,
        pID = taskVars$env$global$pID,
        pName = taskVars$env$global$pName,
        funTasks = list()
      )
      
      # add uIDs?
      if (runTaskOnSet() == TRUE) {
        launchList$uIDs <- moduleManagers()$selectionManager$selectedUIDs()
      }
      
      # add task
      launchList$funTasks[[taskFunction]] <- list(
        funEnv = taskEnvironment(),
        funParams = taskVars$fun,
        taskID = taskID
      )
      
      # get object, add logfiles and save object state
      if (runTaskCombined() == TRUE || runTaskOnSet() == TRUE) {
        curObj <- globalManagers$dataManager()$cciaImageCollection()$cciaObjects()[[uID]]
      } else {
        curObj <- globalManagers$dataManager()$cciaImageCollection()$cciaNodeObjectByUID(uID)
      }
      
      # load object again to get changes from HPC prep
      if (useHPC() == TRUE)
        curObj()$retrieveState(invalidate = FALSE)
      
      # go through lanuch tasks
      subHPCJobs <- c()
      logfilepaths <- list()
      for (i in names(launchList$funTasks)) {
        funTask <- launchList$funTasks[[i]]
        
        # create dummy launcher for directories
        # TODO there should be a cleaner way
        x <- TaskLauncher$new()
        xTaskVars <- taskVars
        xTaskVars$fun <- funTask$funParams
        x$initTask(i, xTaskVars, taskID = funTask$taskID)
        
        if (runTaskOnSet() == FALSE) {
          logfilepaths <- prepLogfilepaths(x, logfilepaths, addTaskAttr = TRUE, removePrevious = TRUE)
          
          if (useHPC() == TRUE) {
            # add to HPC jobs
            subHPCJobs <- c(
              subHPCJobs, x$getHPCConf("hpcSlurmFile")
            )
          }
        }
      }
      
      # save function parameters for image itself
      # ie/ this will help to know which parameters
      # were used for to process this image
      curObj()$saveModuleFunParams(
        taskFunction, taskVars$fun, invalidate = FALSE
      )
      
      curObj()$saveState(saveData = FALSE)
      
      # save state for children if on set
      if (runTaskCombined() == TRUE || runTaskOnSet() == TRUE) {
        for (x in curObj()$cciaObjects(uIDs = moduleManagers()$selectionManager$selectedUIDs())) {
          x()$saveState(saveData = FALSE)
        }
      }
      
      # prepare launchpad
      taskLauncher <- TaskLauncher$new()
      
      # change running environment for launchpad
      taskVars$env$global$env <- "local"
      
      # run together on launchpad or individually?
      if (runTaskOnSet() == TRUE) {
        # get new task id and reset task name
        taskID <- generateTaskID(uID, "launchpad.runCciaSet")
        taskFunction <- "launchpad.runCciaSet"
        
        # add task ID to parameters
        launchList$taskID <- taskID
        
        taskLauncher$initTask(
          "launchpad.runCciaSet", taskVars, taskID = taskID, forceHPCConf = TRUE)
        
        # init logfiles
        # TODO this has to be here, because the task will be run 
        # under the launchpad not the actual function
        # this is confusing but I am not sure how else to do that
        # unless to reroute the logfile output of the invidual objects
        logfilepaths <- prepLogfilepaths(
          taskLauncher, logfilepaths, addTaskAttr = TRUE, removePrevious = TRUE)
        
        if (useHPC() == TRUE) {
          # add to HPC jobs
          subHPCJobs <- c(
            subHPCJobs, taskLauncher$getHPCConf("hpcSlurmFile")
          )
        }
      } else {
        taskLauncher$initTask("launchpad.runCciaObj", taskVars, inplace = FALSE)
        # for debugging
        # taskLauncher$initTask("launchpad.runCciaObj", taskVars, inplace = TRUE)
        
        # add launcher to logfiles
        logfilepaths <- prepLogfilepaths(
          taskLauncher, logfilepaths, addTaskAttr = TRUE, removePrevious = TRUE)
      }
      
      # upload ccia object if HPC
      if (useHPC() == TRUE) {
        hpcTasks <- list()
        
        # remove logfiles
        hpcTasks[["hpc.rmFiles"]] <- list(
          funEnv = "local",
          funParams = list(
            files = lapply(logfilepaths, function(x) attr(x, "remoteLog"))
          )
        )
        
        # upload ccia object
        if (uploadCciaObj() == TRUE) {
          # prepare fun params
          funParams <- list()
          
          if (runTaskCombined() == TRUE || runTaskOnSet() == TRUE) 
            funParams$uIDs <- moduleManagers()$selectionManager$selectedUIDs()
          
          hpcTasks[["hpc.uploadCciaObj"]] <- list(
            funEnv = "local",
            funParams = funParams
          )
        }
        
        # insert into task list
        launchList$funTasks <- append(
          launchList$funTasks,
          hpcTasks,
          after = 0
        )
      }
      
      # set function parameters
      # TODO this has to be here, because rmFiles
      # depends on logfilepaths which might have to be
      # generated if the task is run on the set
      taskLauncher$setFunParams(launchList)
      
      # prepare launchpad run
      taskLauncher$prepRun(uploadSubmissionFiles = FALSE)
      
      # TODO this is not clean
      # but otherwise it will not kill HPC jobs launched by launchpad
      taskLauncher$setSubHPCJobs(subHPCJobs)
      
      # prepare tasklist
      taskList <- list("launchpad" = taskLauncher)
      
      attr(taskList, "uID") <- uID
      attr(taskList, "taskID") <- paste(uID, taskFunction, taskID, sep = ".")
      attr(taskList, "taskFunction") <- taskFunction
      attr(taskList, "logfilepaths") <- logfilepaths
    }

    taskID <- attr(taskList, "taskID")
    
    # check whether the task limits is reached
    if (length(taskMonitorUtils()$runningTasks()) < taskLimit()) {
      # submit task
      # this could be any number of TaskLauncher instances
      taskMonitorUtils()$addTask(taskID, createForkedTask(taskList, poll = cciaConf()$tasks$results$poll),
                               invalidate = FALSE, invalidateInAbscence = TRUE)
      
      # set enviroment
      taskMonitorUtils()$getTask(taskID)$setTaskEnvironment(taskEnvironment())
      taskMonitorUtils()$getTask(taskID)$setUID(attr(taskList, "uID"))
      taskMonitorUtils()$getTask(taskID)$setTaskFunction(attr(taskList, "taskFunction"))
      taskMonitorUtils()$getTask(taskID)$setTaskID(attr(taskList, "taskID"))
      taskMonitorUtils()$getTask(taskID)$setLogfilepaths(attr(taskList, "logfilepaths"))

      # create task observer
      createTaskObserver(taskID)
    } else {
      addTaskToQueue(taskID, taskList)
    }
  }
  
  # get task variables
  taskLauncherVars <- function(uID, taskEnv = "local") {
    createTaskVars(
      uID, globalManagers$projectManager(), taskEnv,
      taskHPCnumNodes = taskHPCnumNodes(),
      taskHPCnumTasks = taskHPCnumTasks(),
      taskHPCnumCPUperTask = taskHPCnumCPUperTask(),
      taskHPCnumGPUperTask = taskHPCnumGPUperTask(),
      taskHPCmemory = taskHPCmemory(),
      taskHPCwalltime = taskHPCwalltime(),
      useGPU = useGPU(),
      useMATLAB = useMATLAB()
    )
  }
  
  # generate new task ID for UID and function
  generateTaskID <- function(uID, taskfunction) {
    counterID <- paste(uID, taskfunction, sep = ".")
    newTaskID <- 1
    
    # increase counter
    if (counterID %in% names(tasksCounter)) {
      newTaskID <- tasksCounter[[counterID]] + 1
    }
    
    # set id
    tasksCounter[[counterID]] <<- newTaskID
    
    newTaskID
  }
  
  # add task to queue
  addTaskToQueue <- function(taskID, taskList = NULL) {
    taskMonitorUtils()$addTask(taskID, taskList, invalidate = FALSE, invalidateInAbscence = TRUE)
  }
  
  # run queued tasks
  runQueuedTasks <- function() {
    if (length(taskMonitorUtils()$queuedTasks()) > 0) {
      # how many can you start?
      tasksToStart <- length(taskMonitorUtils()$queuedTasks())
      tasksToStart <- if (tasksToStart > taskLimit()) taskLimit() else tasksToStart
      
      # get names
      taskIDs <- names(taskMonitorUtils()$queuedTasks())
      
      for (i in seq(tasksToStart)) {
        taskID <- taskIDs[[i]]
        uID <- taskMonitorUtils()$getTaskAttr(taskID, "uID")
        
        # run task
        createNewTask(uID, taskMonitorUtils()$getTask(taskID))
      }
      
      # save task states
      curTaskStates(taskMonitorUtils()$taskStates())
    }
  }
  
  # cancel task
  stopTask <- function(taskID) {
    # is the task running or queued?
    if (taskID %in% names(taskMonitorUtils()$runningTasks())) {
      taskMonitorUtils()$getTask(taskID)$cancel()
      
      # save task states
      curTaskStates(taskMonitorUtils()$taskStates())
    } else if (taskID %in% names(taskMonitorUtils()$queuedTasks())) {
      taskMonitorUtils()$removeTask(taskID)
    }
  }
  
  # stop tasks
  stopTasks <- function(taskIDs) {
    req(all(taskIDs %in% c(
      names(taskMonitorUtils()$runningTasks()),
      names(taskMonitorUtils()$queuedTasks())
      )))
    
    if (length(taskIDs) > 0) {
      # get HPC jobs
      hpcTaskIDs <- c()
      for (taskID in taskIDs[taskIDs %in% names(taskMonitorUtils()$runningTasks())]) {
        if (taskMonitorUtils()$getTask(taskID)$getTaskEnvironment() == "hpc") {
          hpcTaskIDs <- c(hpcTaskIDs, taskID)
        }
      }
      
      # stop local tasks
      for (taskID in taskIDs) {
        stopTask(taskID)
      }
    }
  }
  
  # run tasks
  runTasks <- function(uIDs, taskLists = list(), taskIDs = list()) {
    if (length(uIDs) > 0) {
      # run specific task for the images
      for (uID in uIDs) {
        if (uID %in% names(taskLists)) {
          createNewTask(uID, taskList = taskLists[[uID]])
        } else if (uID %in% names(taskIDs)) {
          createNewTask(uID, taskID = taskIDs[[uID]])
        } else {
          createNewTask(uID)
        }
      }
      
      # save task states
      curTaskStates(taskMonitorUtils()$taskStates())
    }
  }
  
  # create task observer
  createTaskObserver <- function(taskID) {
    o <- observe({
      # Only proceed when the task is completed (this could mean success,
      # failure, or cancellation)
      req(taskID %in% names(taskMonitorTaskList()))
      req(taskMonitorUtils()$getTask(taskID)$completed())
      
      if (DEBUG_SHOW_TASK_RESULT == TRUE) {
        if (!is.null(taskMonitorUtils()$getTask(taskID)$result())) {
          cat(file = stderr(),
              ">> Task result for", taskID ," > ", taskMonitorUtils()$getTask(taskID)$result())
        }
      }
      
      # add to success or failed
      if (taskMonitorUtils()$getTask(taskID)$getState() == "success") {
        # get uID
        uID <- taskMonitorUtils()$getTask(taskID)$uID()
        
        # get object and load state
        # TODO this should be better to check whether
        # the returning uID is a set or an image
        if (uID %in% globalManagers$dataManager()$cciaImageCollection()$cciaObjectUIDs()) {
          globalManagers$dataManager()$cciaImageCollection()$cciaObjectByUID(uID)[[1]]()$retrieveState(invalidate = FALSE)
        } else {
          globalManagers$dataManager()$cciaImageCollection()$cciaNodeObjectByUID(uID)()$retrieveState(invalidate = FALSE)
        }
        
        # reload image in viewer?
        if (input$taskUpdateImage == TRUE) {
          if (!is.null(moduleManagers()$imageViewerManager$shownImage())) {
            if (runTaskCombined() == TRUE || runTaskOnSet() == TRUE) {
              # the whole set changed - update the image
              moduleManagers()$imageViewerManager$updateImage()
            } else {
              # check if the task was for the shown image
              if (uID == moduleManagers()$imageViewerManager$shownImage()$getUID()) {
                moduleManagers()$imageViewerManager$updateImage()
              }
            }
          }
        }
      }
      
      # save task states
      curTaskStates(taskMonitorUtils()$taskStates())
      
      # run next in queue
      runQueuedTasks()
      
      # This observer only runs once
      o$destroy()
    })
  }
  
  # reset task lists
  resetTaskList <- function() {
    # reset lists
    taskMonitorUtils()$setTaskList(list())
  }
  
  # get job states from directory
  getJobStates <- function() {
    # create sshUtil
    sshUtil <- SshUtils$new(projectManager = globalManagers$projectManager)
    
    sshUtil$sshExecute(
      sprintf("du -a %s",
              paste(
                globalManagers$projectManager()$hpcProjectDirectory(),
                "*",
                cciaConf()$dirs$tasks$tasks,
                sep = "/"
              )
      )
    )
  }
  
  # destroy log observers
  destroyTaskLogObservers <- function() {
    # reset log observers
    for (i in names(taskLogObservers)) {
      # destroy observer
      taskLogObservers[[i]]$destroy()
      
      # cancel listener
      if (i %in% names(taskLogListeners))
        taskLogListeners[[i]]$kill()
    }
  }
  
  ### Observers - RxAction
  ## Event specific
  # check inputs
  observeEvent(c(
    taskHPCnumNodes(),
    taskHPCnumTasks(),
    taskHPCnumCPUperTask(),
    taskHPCnumGPUperTask(),
    taskHPCmemory(),
    taskHPCwalltime()
    ), {
    checkInputs <- c(
      "taskHPCnumNodes",
      "taskHPCnumTasks",
      "taskHPCnumCPUperTask",
      "taskHPCnumGPUperTask",
      "taskHPCmemory",
      "taskHPCwalltime"
    )
      
    for (i in checkInputs) {
      if (is.null(eval(parse(text = paste(i, "()"))))) {
        addClass(i, formINPUT_ERROR_CLASS)
      } else {
        removeClass(i, formINPUT_ERROR_CLASS)
      }
    }
  })
  
  # listen to changes in environment for selected function
  observeEvent(c(
    taskFunctionEnvironments(),
    taskEnvironment()), {
    # update panel if environment not in selected
    if (!(taskEnvironment() %in% taskFunctionEnvironments())) {
      updateTabsetPanel(
        session, "taskEnvironment",
        selected = taskFunctionEnvironments()[[1]]
      )
    }
  })
  
  # clear tasks
  observeEvent(input$taskClearTasks, {
    # clear list
    taskMonitorUtils()$clearTasks()
  })
  
  # run selected
  observeEvent(input$taskRunSelected, {
    req(moduleManagers()$imageSetManager$selectedSet())
    
    # get row numbers
    if (runTaskCombined() == TRUE || runTaskOnSet() == TRUE) {
      uIDs <- c(moduleManagers()$imageSetManager$selectedSet()$getUID())
    } else {
      uIDs <- moduleManagers()$selectionManager$selectedUIDs()
      
      # make sure that it only runs tasks that are shown in the current set
      uIDs <- uIDs[
        uIDs %in% names(moduleManagers()$imageSetManager$selectedSet()$cciaObjects())]
    }
    
    # run tasks
    runTasks(uIDs)
  })
  
  # stop all processes
  observeEvent(input$taskCancelAll, {
    # get row numbers
    # uIDs <- managerConf$imageData()$uID
    
    # get only busy tasks
    taskIDs <- c(
      names(taskMonitorUtils()$runningTasks()),
      names(taskMonitorUtils()$queuedTasks())
    )
    
    stopTasks(taskIDs)
  })
  
  # cancel selected
  # observeEvent(input$taskCancelSelected, {
  #   # get row numbers
  #   uIDs <- moduleManagers()$selectionManager$selectedUIDs()
  #   
  #   stopTasks(uIDs)
  # })
  
  # cancel shown tasks
  observeEvent(input$taskCancelShown, {
    req(taskMonitorAttr())

    stopTasks(taskMonitorAttr()$name)
  })
  
  # toggle selected process
  observeEvent(input$toggleProcess, {
    # get row number
    taskID <- input$toggleProcess
    
    # is it running or queued?
    if (taskID %in% c(
      names(taskMonitorUtils()$runningTasks()), names(taskMonitorUtils()$queuedTasks()))) {
      # stop tasks
      stopTasks(c(taskID))
      
      # run queued
      runQueuedTasks()
    } else {
      # run tasks
      # TODO run the old task .. ?
      # taskHandle <- getTaskHandle(taskID)
      # uID <- taskHandle$uID()
      # taskLists <- list()
      # taskLists[[uID]] <- taskHandle$tasks()
      
      # runTasks(c(uID), taskLists = taskLists)
      # create a new task for that old task with the same id
      uID <- taskMonitorUtils()$getTask(taskID)$uID()
      
      # run task again with new settings if function matches
      # else run task with old settings
      if (taskMonitorUtils()$getTask(taskID)$taskFunction() == taskFunction()) {
        taskIDs <- list()
        taskIDs[[uID]] <- taskMonitorUtils()$getTask(taskID)$taskID(fullID = FALSE)
        
        runTasks(c(uID), taskIDs = taskIDs)
      } else {
        taskLists <- list()
        taskLists[[uID]] <- taskMonitorUtils()$getTask(taskID)$tasks()
        
        # go through and prep runs again
        for (x in taskLists[[uID]]) {
          x$prepRun(uploadSubmissionFiles = FALSE)
        }
        
        runTasks(c(uID), taskLists = taskLists)
      }
    }
  })
  
  # create panels
  observeEvent(taskLogfilepaths(), {
    # to be save
    destroyTaskLogObservers()
    
    taskLogObservers <<- list()
    taskLogListeners <<- list()
    taskLogContent <<- list()
    
    for (x in names(taskLogfilepaths())) {
      # init content
      outputFile <- taskLogfilepaths()[[x]]
      taskLogContent[[x]] <<- reactiveVal()
      
      local({
        local_x <- x
        local_outputFile <- outputFile
        local_attributes <- attributes(taskLogfilepaths())
        local_remoteLog <- attr(local_outputFile, "remoteLog")
        local_init = TRUE
        
        # create observers
        taskLogObservers[[local_x]] <<- observe({
          if (!is.null(local_outputFile)) {
            # get log
            taskLogContent[[local_x]](readLogFile(
              local_outputFile, previousContent = taskLogContent[[local_x]](),
              mergeContent = !local_init
            ))
            
            # reset init
            local_init <- FALSE
            
            # TODO only update the latest information
            if (!is.null(taskLogContent[[local_x]]()) &&
                attr(taskLogContent[[local_x]](), "updated") == TRUE) {
              html(id = sprintf("logOutputPanel_%s", local_x),
                            # html = attr(taskLogContent[[local_x]](), "updatedContent"),
                            # add = TRUE)
                            # html = taskLogContent[[local_x]]())
                            html = trimws(paste(taskLogContent[[local_x]](), collapse = "\n")))
            }
          }
          
          invalidateLater(cciaConf()$tasks$log$poll)
        })
        
        # create listener for file changes
        # TODO this is slurm specific
        # if (local_attributes$taskEnv == "hpc" || endsWith(local_outputFile, ".slurm.log")) {
        if (!is.null(local_remoteLog)) {
          # get environment variables
          envVars <- createTaskVars(
            local_attributes$uID,
            globalManagers$projectManager(),
            local_attributes$taskEnv
          )$env
          
          # run task
          taskLogListeners[[local_x]] <<- taskLogObj()$runTask(
            "hpc.tailFile",
            funParams = list(
              input = local_remoteLog,
              output = local_outputFile
            ),
            envVars = envVars,
            hpcDir = dirname(dirname(envVars$hpc$dirs$task)),
            env = "local",
            runInplace = FALSE
          )
        }
      })
    }
  })
  
  # close log
  observeEvent(input$closeLog, {
    req(taskLogfilepaths())
    
    # destroy observers
    destroyTaskLogObservers()
    
    # close dialog
    removeModal()
  })
  
  # show log of selected process
  observeEvent(input$showLog, {
    req(!input$showLog %in% names(taskMonitorUtils()$queuedTasks()))
    
    # get row number
    taskID <- input$showLog
    uID <- taskMonitorUtils()$getTask(taskID)$uID()
    
    # is it running or finished?
    if (taskID %in% c(
      names(taskMonitorUtils()$runningTasks()), names(taskMonitorUtils()$successTasks()), names(taskMonitorUtils()$failedTasks()))) {
      # set task object to show logs for
      if (runTaskCombined() == TRUE || runTaskOnSet() == TRUE) {
        taskLogObj(
          moduleManagers()$imageSetManager$selectedSet()
        )
      } else {
        taskLogObj(
          moduleManagers()$imageSetManager$selectedSet()$cciaObjectByUID(uID)[[1]]()
        )
      }

      # show log
      showModal(modalDialog(
        fluidPage(
          fluidRow(
            column(
              11, tags$label(sprintf("Logs for %s", taskID))
            ),
            column(
              1,
              actionButton(session$ns("closeLog"), "Close"),
              align = "right"
            )
          ),
          fluidRow(
            # column(12,
            #        div(style = "max-height:500px;overflow-y:scroll",
            #            uiOutput(session$ns("logOutput")))
            # )
            column(12, uiOutput(session$ns("logOutput")))
          )
        ),
        footer = tagList(),
        easyClose = FALSE, size = "l"
      ))
    }
  })
  
  # save task states for processes
  # observeEvent(c(
  #   taskMonitorUtils()$runningTasks(), taskMonitorUtils()$queuedTasks(),
  #   taskMonitorUtils()$failedTasks(), taskMonitorUtils()$successTasks()), {
  #     curTaskStates(taskMonitorUtils()$taskStates())
  # })
  
  # has the task limit been adjusted?
  observeEvent(taskLimit(), {
    runQueuedTasks()
  })
  
  # are tasks running?
  observeEvent(taskMonitorAttr(), {
    if (!is.null(taskMonitorAttr())) {
      enable("taskCancelShown")
    } else {
      disable("taskCancelShown")
    }
  })
  
  # are rows selected?
  observeEvent(moduleManagers()$selectionManager$numSelectedUIDs(), {
    if (moduleManagers()$selectionManager$numSelectedUIDs() > 0) {
      # enable or disable buttons
      enable("taskRunSelected")
      
      # are tasks busy?
      if(numTotalBusyTasks() > 0) {
        enable("taskCancelSelected")
      }
    } else {
      disable("taskRunSelected")
    }
  })
  
  # is the environment toggled?
  observeEvent(taskEnvironment(), {
    # # cancel all jobs
    # uIDs <- managerConf$imageData()$uID
    # 
    # # stop tasks
    # stopTasks(uIDs)
    # 
    # resetTaskList()
  })
  
  # sync job status with HPC
  # TODO does this work better?
  observeEvent(taskHPCsync(), {
    progress <- Progress$new()
    progress$set(message = "Get HPC job states ... ", value = 20)
    
    jobstates <- getJobStates()
    
    # filter for log files
    jobstates <- jobstates[!is.na(stringr::str_match(jobstates, ".job_id$"))]
    
    # get jobs for current module
    funModule <- sub("\\..*", "", taskFunction()) 
    jobstates <- jobstates[!is.na(stringr::str_match(jobstates, funModule))]
    
    # get uIDs
    uIDs <- c(stringr::str_match(
      jobstates,
      sprintf(
        "(?<=%s\\/%s/%s\\/)[:alnum:]{6}",
        globalManagers$projectManager()$getProjectUID(),
        cciaConf()$hpc$dirs$analysis,
        globalManagers$projectManager()$getProjectVersionID()
        )))
    
    # get task functions
    tfNames <- stringr::str_match(
      jobstates, sprintf("(?<=[:alnum:]{6}/%s/%s.)[^\\.]*(?=\\.)",
                         cciaConf()$dirs$tasks$tasks,
                         funModule))
    tfNames <- paste(funModule, tfNames, sep = ".")
    
    # get job files
    jobIDfiles <- stringr::str_match(jobstates, "/.*")
    
    # get taskIDs
    taskIDs <- stringr::str_extract(basename(jobIDfiles), "(?<=\\.)[0-9]+(?=\\.)")
    
    # remove na
    uIDs <- uIDs[!is.na(uIDs)]
    tfNames <- tfNames[!is.na(uIDs)]
    taskIDs <- taskIDs[!is.na(uIDs)]
    jobIDfiles <- jobIDfiles[!is.na(uIDs)]
    
    # compile task IDs
    taskIDs.compiled <- paste(uIDs, tfNames, taskIDs, sep = ".")
    
    # get cancelled jobs
    progress$set(message = "Get cancelled jobs ... ", value = 50)
    
    cancelledIDs <- cancelledTaskIDs(taskIDs.compiled, jobIDfiles)
    
    # resume jobs
    cancelledTasks <- list()
    if (length(uIDs) > 0) {
      for (i in seq(length(uIDs))) {
        # taskID <- taskIDs[i]
        uID <- uIDs[i]
        taskID.compiled <- taskIDs.compiled[i]
        
        # was the job cancelled by the scheduler?
        if (taskID.compiled %in% cancelledIDs) {
          taskMonitorUtils()$getTask(taskID.compiled)$cancel()
          
          # run next in queue
          runQueuedTasks()
        } else {
          # watch for job exit
          createNewTask(uID, taskID = taskID.compiled, hpcExitOnly = TRUE)
        }
      }
    } else {
      resetTaskList()
    }
    
    progress$close()
  })
  
  # toggle processing buttons
  observeEvent(changedTaskStates(), {
    req(any(lengths(changedTaskStates()) > 0))
    
    # queued task
    for (taskID in changedTaskStates()$queudIDs) {
      toggleButtonType(
        taskID, btnLABEL_QUEUED, btnCLASS_WARNING)
    }
    
    # running task
    for (taskID in changedTaskStates()$runningIDs) {
      toggleButtonType(
        taskID, btnLABEL_RUNNING, btnCLASS_RUNNING)
    }
    
    # success task
    for (taskID in changedTaskStates()$successIDs) {
      toggleButtonType(
        taskID, btnLABEL_SUCCESS, btnCLASS_SUCCESS)
    }
    
    # failed task
    for (taskID in changedTaskStates()$failedIDs) {
      toggleButtonType(
        taskID, btnLABEL_FAILED, btnCLASS_IMPORTANT)
    }
    
    # set previous states
    prevTaskStates(curTaskStates())
  })
  
  ### Functions
  
  # create invidividual buttons for task management
  createTaskDataButtons <- function(taskID) {
    # buttons
    isolate(procButtons <- initProcessingButtons(taskID))
    
    # columns
    buttons <- list(
      actionButton(
        paste0(session$ns("toggleProcess_"), taskID),
        label = procButtons$labels[[1]],
        class = procButtons$classes[[1]],
        onclick = paste(
          sprintf(
            'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
            session$ns("toggleProcess"),
            taskID
          )
        )
      ),
      actionButton(
        paste0(session$ns("showLog_"), taskID),
        label = "LOG",
        onclick = paste(
          sprintf(
            'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
            session$ns("showLog"),
            taskID
          )
        )
      )
    )
    
    tagList(buttons)
  }
  
  # create columns for task management
  createTaskDataTableColumns <- function(taskIDs = NULL, showToggle = TRUE, showLog = TRUE) {
    # buttons
    isolate({
      procButtons <- initProcessingButtons(taskIDs)
    })

    if (is.null(taskIDs)) {
      taskIDs <- managerConf$imageData()$taskID
    }

    # columns
    taskCols <- list()
    
    if (showToggle == TRUE) {
      taskCols <- append(taskCols, list(
        " " = shinyInput(
          "actionButton", session$ns("toggleProcess_"), taskIDs,
          initLabel = procButtons$labels,
          initClass = procButtons$classes,
          initOnclick = paste(
            sprintf(
              'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
              session$ns("toggleProcess"),
              taskIDs
            )
          )
        )
      ))
    }
    
    if (showLog == TRUE) {
      taskCols <- append(taskCols, list(
        " " = shinyInput(
          "actionButton", session$ns("showLog_"), taskIDs,
          initLabel = rep("LOG", length(taskIDs)),
          initOnclick = paste(
            sprintf(
              'Shiny.setInputValue(\"%s\", "%s", {priority: "event"})',
              session$ns("showLog"),
              taskIDs
            )
          )
        )
      ))
    }
    
    taskCols
  }
  
  # init processing labels
  initProcessingButtons <- function(taskIDs) {
    curStates <- taskMonitorUtils()$taskStates()
      
    curLabels <- rep("RUN", length(taskIDs))
    curClasses <- rep("", length(taskIDs))
    
    # names
    names(curLabels) <- taskIDs
    names(curClasses) <- taskIDs
      
    # labels
    curLabels[names(curLabels) %in% curStates$runningIDs] <-
      btnLABEL_RUNNING
    curLabels[names(curLabels) %in% curStates$queudIDs] <-
      btnLABEL_QUEUED
    curLabels[names(curLabels) %in% curStates$successIDs] <-
      btnLABEL_SUCCESS
    curLabels[names(curLabels) %in% curStates$failedIDs] <-
      btnLABEL_FAILED

    # classed
    curClasses[names(curClasses) %in% curStates$runningIDs] <-
      btnCLASS_RUNNING
    curClasses[names(curClasses) %in% curStates$queudIDs] <-
      btnCLASS_WARNING
    curClasses[names(curClasses) %in% curStates$successIDs] <-
      btnCLASS_SUCCESS
    curClasses[names(curClasses) %in% curStates$failedIDs] <-
      btnCLASS_IMPORTANT
    
    list(
      labels = curLabels,
      classes = curClasses
    )
  }
  
  # toggle button type
  toggleButtonType <- function(taskID, btnLabel, selectedClass) {
    btnID <- paste0("toggleProcess_", taskID)
    
    # update label
    html(btnID, btnLabel)
    
    allClasses <- c(
      btnCLASS_SUCCESS, btnCLASS_RUNNING,
      btnCLASS_IMPORTANT, btnCLASS_WARNING
    )
    
    if (!is.null(selectedClass)) {
      addClass(btnID, selectedClass)
      
      # remove non selected class
      allClasses <- allClasses[allClasses != selectedClass]
    }
    
    # toggle classes
    for (i in allClasses) {
      removeClass(btnID, i)
    }
  }
  
  ## HPC
  ### Functions
  # which jobs were cancelled?
  cancelledTaskIDs <- function(taskIDs, jobIDfiles) {
    failedTaskIDs <- c()
    
    if (length(taskIDs) > 0) {
      # collate all job ids into one file
      cmd <- c(sprintf("cd %s", globalManagers$projectManager()$hpcProjectDirectory()))
      
      # https://stackoverflow.com/a/31228590/13766165
      cmd <- c(cmd, sprintf(
        "paste -d, %s > ./query_job_ids",
        paste(
          jobIDfiles, collapse = " "
        )))
      
      # get job info for taskIDs
      cmd <- c(cmd, "xargs -a query_job_ids sacct --format=jobid,state -j ")
      
      # create sshUtil
      sshUtil <- SshUtils$new(projectManager = globalManagers$projectManager)
      
      # submit request
      jobInfo <- sshUtil$sshExecute(paste(cmd, collapse = "; "))
      
      # get job ID
      jobIDx <- stringr::str_match(jobInfo, "[0-9]+ ")
      
      # get state
      jobStatus <- c(stringr::str_match(jobInfo[!(is.na(jobIDx))], "[:alpha:]+"))
      names(jobStatus) <- taskIDs
      
      # filter IDs
      jobsCancelled <- jobStatus[jobStatus %in% c("CANCELLED", "FAILED")]
      
      failedTaskIDs <- names(jobsCancelled)
    }
    
    failedTaskIDs
  }
  
  ## Output
  # task monitor select uIDs
  output$taskMonitorUIDs <- renderUI({
    containerAttr <- list()
    
    if (length(taskMonitorTaskList()) > 0) 
      containerAttr <- unique(taskMonitorTaskDT()$uID)
    
    createSelectInput(
      session$ns("taskMonitorUIDs"),
      label = "uIDs",
      choices =  containerAttr,
      multiple = TRUE,
      selected = input$taskMonitorUIDs
    )
  })
  
  # task monitor select task functions
  output$taskMonitorTaskFunctions <- renderUI({
    containerAttr <- list()
    
    if (length(taskMonitorTaskList()) > 0)
      containerAttr <- unique(taskMonitorTaskDT()$taskFunction)
    
    createSelectInput(
      session$ns("taskMonitorTaskFunctions"),
      label = "Functions",
      choices = containerAttr,
      multiple = TRUE,
      selected = input$taskMonitorTaskFunctions
    )
  })
  
  # set task limits
  output$taskLimit <- renderUI({
    req(taskFunction())
    
    sliderInput(
      session$ns("taskLimit"), "Tasks",
      min = 1,
      max = taskFunctionTasksLimit(),
      value = 1, 
      step = 1
      )
  })
  
  # set task functions
  output$taskFunction <- renderUI({
    selectInput(
      session$ns("taskFunction"), managerConf$task$funLabel,
      choices = taskFunctionList()
    )
  })
  
  # show parameters based on selected workflow
  output$funParams <- renderUI({
    req(funParamsUI())
    
    funParamsUI()
  })
  
  observeEvent(c(cciaObj(), taskFunction()), {
    req(cciaObj())
    
    # set ccia object and set
    if (is.null(moduleManagers()$inputManager$cciaObject()) ||
        cciaObj()$getUID() != moduleManagers()$inputManager$cciaObject()$getUID()) {
      moduleManagers()$inputManager$setCciaObject(cciaObj())
      moduleManagers()$inputManager$setCciaObjectSet(
        moduleManagers()$imageSetManager$selectedSet()
      )
      moduleManagers()$inputManager$setCciaObjectCollection(
        globalManagers$dataManager()$cciaImageCollection()
      )
    }
    
    # TODO can you add observers generated from the panels here .. ?
    funParamsOut <- moduleManagers()$inputManager$createFunParamsUI(taskFunction())
    
    # set UI
    funParamsUI(funParamsOut$ui)
    
    # execute observers
    if (length(funParamsOut$observers) > 0) {
      # go through groups
      for (i in names(funParamsOut$observers)) {
        x <- funParamsOut$observers[[i]]
        
        # go through observers
        # TODO there is probably only one per group?
        for (j in names(x)) {
          obsNs <- environment(session$ns)[["namespace"]]
          inputName <- trimInputName(obsNs, j)
          groupName <- stringr::str_sub(
            inputName, start = 1, end = stringi::stri_locate_last_fixed(inputName, "_")[,1] - 1)
          
          observeEvent(input[[inputName]], {
            # define local variables
            ns_local <- local(obsNs)
            inputName_local <- local(inputName)
            groupName_local <- local(groupName)
            self_local <- local(moduleManagers()$inputManager)
            vars_local <- local(if ("vars" %in% names(funParamsOut)) funParamsOut$vars[[i]] else NULL)
            
            eval(x[[j]])
          })
        }
      }
    }
  })
  
  # output for log
  output$logOutput <- renderUI({
    req(taskLogfilepaths())
    
    # create panels
    tabPanels <- lapply(taskLogfilepaths(), function(x) {
      tabPanel(
        basename(x), value = basename(x),
        div(style = "max-height:500px;overflow-y:scroll",
            verbatimTextOutput(
              session$ns(sprintf("logOutputPanel_%s", basename(x)))
              )
        )
      )
    }) %>% unname
    
    # show panels
    fluidRow(
      do.call(
        tabsetPanel, c(
          tabPanels,
          id = session$ns("logOutputPanels"),
          type = "tabs"
        )
      )
    )
  })
  
  # get attributes from task container
  getContainerAttributes = function(taskContainer, attrToGet = c("uID", "taskFunction", "taskID"), asDT = FALSE) {
    # check whether task is a list or already a forked task
    isTaskList <- sapply(taskContainer, function(x) if (is.null(attr(x, "uID"))) FALSE else TRUE)
    
    attrList <- list()
    
    # go through attributes and add to list
    if ("uID" %in% attrToGet) {
      attrList$uID <- unlist(sapply(taskContainer[isTaskList], function(x) attr(x, "uID")))
      attrList$uID[!isTaskList] <- unlist(sapply(taskContainer[!isTaskList], function(x) x$uID()))
    }
    if ("taskFunction" %in% attrToGet) {
      attrList$taskFunction <- unlist(sapply(taskContainer[isTaskList], function(x) attr(x, "taskFunction")))
      attrList$taskFunction[!isTaskList] <- unlist(sapply(taskContainer[!isTaskList], function(x) x$taskFunction()))
    }
    if ("taskID" %in% attrToGet) {
      attrList$taskID <- unlist(sapply(taskContainer[isTaskList], function(x) 
        stringr::str_extract(attr(x, "taskID"), "(?<=\\.)[:alnum:]+$")))
      attrList$taskID[!isTaskList] <- unlist(sapply(taskContainer[!isTaskList], function(x) x$taskID(fullID = FALSE)))
    }
    
    # convert to DT?
    if (asDT == TRUE) {
      attrList <- as.data.table(do.call(cbind, attrList))
      
      # add names
      attrList$name <- names(taskContainer)
    }
    
    attrList
  }
  
  # table outputs for monitor
  output$taskMonitorAll <- DT::renderDataTable({
    req(taskMonitorTable())
    
    taskMonitorTable()
  })
  
  # output$taskMonitorRunning <- DT::renderDataTable({
  #   req(length(taskMonitorUtils()$runningTasks()) > 0 || length(taskMonitorUtils()$queuedTasks()) > 0)
  #   
  #   createTaskMonitorTable(
  #     append(taskMonitorUtils()$runningTasks(), taskMonitorUtils()$queuedTasks())
  #   )
  # })
  
  # output$taskMonitorQueued <- DT::renderDataTable({
  #   req(length(queuedTasks()) > 0)
  #   
  #   createTaskMonitorTable(queuedTasks())
  # })
  
  # output$taskMonitorSuccess <- DT::renderDataTable({
  #   req(length(taskMonitorUtils()$successTasks()) > 0)
  #   
  #   createTaskMonitorTable(taskMonitorUtils()$successTasks())
  # })
  # 
  # output$taskMonitorFailed <- DT::renderDataTable({
  #   req(length(taskMonitorUtils()$failedTasks()) > 0)
  #   
  #   createTaskMonitorTable(taskMonitorUtils()$failedTasks())
  # })
  
  ## public functions
  list(
    createTaskDataTableColumns = createTaskDataTableColumns,
    createTaskDataButtons = createTaskDataButtons,
    useGPU = useGPU,
    useHPC = useHPC,
    taskHPCnumNodes = taskHPCnumNodes,
    taskHPCnumTasks = taskHPCnumTasks,
    taskHPCnumGPUperTask = taskHPCnumGPUperTask,
    taskHPCnumGPUperTask = taskHPCnumGPUperTask,
    taskHPCmemory = taskHPCmemory,
    taskHPCwalltime = taskHPCwalltime,
    deleteTaskDirectory = deleteTaskDirectory
  )
}
