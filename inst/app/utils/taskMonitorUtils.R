TaskMonitorUtils <- R6::R6Class(
  "TaskMonitorUtils",
  inherit = cecelia::ReactiveObject,
  
  ## private
  private = list(
    taskList = list()
    
    # setters
    # getters
  ),
  
  ### public
  public = list(
    initialize = function() {
    },
    
    # return task states
    runningTasks = function() {
      if (length(self$getTaskList()) > 0)
        self$getTaskList()[sapply(self$getTaskList(), function(x)
          if (is.null(attr(x, "uID"))) !x$completed() else FALSE, USE.NAMES = FALSE, simplify = TRUE)]
      else list()
    },
    
    queuedTasks = function() {
      if (length(self$getTaskList()) > 0)
        self$getTaskList()[sapply(self$getTaskList(), function(x)
          if (!is.null(attr(x, "uID"))) TRUE else FALSE, USE.NAMES = FALSE, simplify = TRUE)]
      else list()
    },
    
    successTasks = function() {
      if (length(self$getTaskList()) > 0)
        self$getTaskList()[sapply(self$getTaskList(), function(x)
          if (is.null(attr(x, "uID"))) x$getState() == "success" && x$completed() else FALSE, USE.NAMES = FALSE, simplify = TRUE)]
      else list()
    },
    
    failedTasks = function() {
      if (length(self$getTaskList()) > 0)
        self$getTaskList()[sapply(self$getTaskList(), function(x)
          if (is.null(attr(x, "uID"))) x$getState() %in% c("error", "cancel") else FALSE, USE.NAMES = FALSE, simplify = TRUE)]
      else list()
    },
    
    # add task to list
    addTask = function(taskID, x, invalidate = TRUE, invalidateInAbscence = FALSE) {
      # set invalidate
      if (invalidateInAbscence == TRUE && !taskID %in% names(private$taskList))
          invalidate = TRUE
        
      private$taskList[[taskID]] <- x
      private$invalidate(invalidate = invalidate)
    },
    
    # add task attribute
    addTaskAttr = function(taskID, attrName, x, invalidate = TRUE) {
      attr(private$taskList[[taskID]], attrName) <- x
      private$invalidate(invalidate = invalidate)
    },
    
    # get task attribute
    getTaskAttr = function(taskID, attrName) {
      attr(private$taskList[[taskID]], attrName)
    },
    
    # remove task from list
    removeTask = function(taskID, invalidate = TRUE) {
      private$taskList <- private$taskList[names(private$taskList) != taskID]
      private$invalidate(invalidate = invalidate)
    },
    
    # clear tasks
    clearTasks = function(runningTasks = FALSE, invalidate = TRUE) {
      # remove all tasks except running tasks
      private$taskList <- private$taskList[
        names(private$taskList) %in% c(
          names(self$runningTasks()),
          names(self$queuedTasks())
          )]
      private$invalidate(invalidate = invalidate)
    },
    
    # get task
    getTask = function(taskID) {
      if (taskID %in% names(self$getTaskList())) {
        self$getTaskList()[[taskID]]
      } else {
        NULL
      }
    },
    
    # get task states
    taskStates = function() {
      # get IDs
      runningIDs <- names(self$runningTasks())
      queudIDs <- names(self$queuedTasks())
      successIDs <- names(self$successTasks())
      failedIDs <- names(self$failedTasks())
      
      # return states
      list(
        runningIDs = runningIDs,
        queudIDs = queudIDs,
        successIDs = successIDs,
        failedIDs = failedIDs
      )
    },
    
    # setters
    setTaskList = function(x, invalidate = TRUE) {
      private$taskList <- x
      private$invalidate(invalidate = invalidate)
    },
    
    # getters
    getTaskList = function() {
      private$taskList
    }
  )
)
