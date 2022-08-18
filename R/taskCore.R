#' Core class to execute module tasks
#' 
#' @name TaskCore
#' @description Core class for task functions
#'
#' @examples
#' TODO
#' @export
TaskCore <- R6::R6Class(
  "TaskCore",
  
  private = list(
    taskConf = NULL,
    
    #' @descriptio Parent module
    #' @param moduleName character of module name
    #' @param funName character of functions name
    parentModule = function(moduleName, funName = NULL) {
      # get parent
      moduleName <- stringr::str_split(moduleName, "\\.")[[1]][[1]]
      
      # add function?
      if (!is.null(funName)) {
        moduleName <- paste(moduleName, funName, sep = ".")
      }
      
      moduleName
    },
    
    ## getters
    getTaskConf = function(remoteAsLocal = FALSE) {
      taskConf <- private$taskConf
      
      # set remote as local if running remote
      if (remoteAsLocal == TRUE) {
        taskConf$env$local$dirs <- taskConf$env$hpc$dirs
      }
      
      taskConf
    },
    
    ## setters
    setTaskConf = function(x) {
      private$taskConf <- x
    }
  ),
  
  public = list(
    #' @description Init
    initialize = function() {
    },
  
    #' @description Call other task processes
    #' @param tasks list of character for task functions
    #' @param taskConf list of list of character for task config
    #' @param funParams list of generic for task parameters
    #' @param runLocal boolean to run task local
    #' @param callingEnv character for calling environment
    runTasks = function(tasks, taskConf = NULL, funParams = list(), runLocal = FALSE,
                        callingEnv = NULL) {
      if (is.null(taskConf)) {
        taskConf <- private$getTaskConf()
      }
      
      # add function parameters?
      if (length(funParams) > 0) {
        # this will overwrite ccia params
        # taskConf$fun <- funParams
        for (i in names(funParams)) {
          taskConf$fun[[i]] <- funParams[[i]]
        }
      }
      
      # run other preparations if necessary
      if (taskConf$env$global$env == "hpc" && runLocal == FALSE) {
        taskConf$env$local$dirs$task <- taskConf$env$hpc$dirs$task
        taskConf$env$local$dirs$zero <- taskConf$env$hpc$dirs$zero
      }
      
      # run environment
      taskConf$env$global$env <- "local"
      taskConf$env$global$callingEnv <- self$taskEnv()
      
      for (x in tasks) {
        # init task launcher
        taskLauncher <- TaskLauncher$new()
        
        # run environment
        taskLauncher$initTask(x, taskConf, inplace = TRUE)
        
        # prep run
        taskLauncher$prepRun()
        
        # run task
        taskLauncher$run()
        
        taskLauncher$result(TRUE)
      }
    },
    
    ## setters
    # function parameters
    setFunParams = function(x) {
      if (!is.null(private$getTaskConf())) {
        taskConf <- private$getTaskConf()
        
        # exchange fun
        taskConf$fun <- x
        
        # save back
        private$setTaskConf(taskConf)
      }
    },
    
    ## getters
    
    # function parameters
    funParams = function() {
      if (!is.null(private$getTaskConf())) {
        private$getTaskConf()$fun
      }
    },
    
    # task environment
    taskEnv = function() {
      self$globalParams()$env
    },
    
    # calling environment
    callingEnv = function() {
      if ("callingEnv" %in% names(self$globalParams()))
        self$globalParams()$callingEnv
      else
        self$taskEnv()
    },
    
    # environment parameters
    envParams = function(taskEnv = NULL) {
      if (!is.null(private$getTaskConf())) {
        if (is.null(taskEnv)) {
          taskEnv <- self$taskEnv()
        }
        
        # get variables for current running environment
        private$getTaskConf()$env[[taskEnv]]
      }
    },
    
    # global parameters
    globalParams = function() {
      if (!is.null(private$getTaskConf())) {
        if ("global" %in% names(private$getTaskConf()$env)) {
          private$getTaskConf()$env$global
        }
      }
    },
    
    # utils parameters
    utilsParams = function() {
      if (!is.null(private$getTaskConf())) {
        if ("utils" %in% names(private$getTaskConf()$env)) {
          private$getTaskConf()$env$utils
        }
      }
    }
  )
)
