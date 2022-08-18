#' @description
#' Adapted from
#' https://gist.github.com/jcheng5/9504798d93e5c50109f8bbaec5abe372
#' Also uses parallel, shinyjs, tools
#'
#' Create a long-running task, executed in a forked process. (Doesn't work on Windows)
#' 
#' The return value is a promise-like object with three
#' methods:
#' - completed(): FALSE initially, then TRUE if the task succeeds,
#'   fails, or is cancelled. Reactive, so when the state changes
#'   any reactive readers will invalidate.
#' - result(): Use this to get the return value. While execution is
#'   in progress, performs a req(FALSE). If task succeeded, returns
#'   the return value. If failed, throws error. Reactive, so when
#'  the state changes any reactive readers will invalidate.
#' - cancel(): Call this to prematurely terminate the task.
#' 
#' @param tasks list of TaskLauncher
#' @param poll integer to poll for task completion
#' @export
createForkedTask <- function(tasks, poll = 500) {
  makeReactiveBinding("state")
  makeReactiveBinding("taskEnvironment")
  makeReactiveBinding("taskID")
  makeReactiveBinding("taskFunction")
  makeReactiveBinding("logfilepaths")
  makeReactiveBinding("uID")
  makeReactiveBinding("taskHandle")
  
  state <- factor(
    "running",
    levels = c("running", "success", "error", "cancel"),
    ordered = TRUE
  )
  
  taskEnvironment <- factor(
    "local",
    levels = c("local", "hpc"),
    ordered = TRUE
  )
  
  taskID <- NULL
  taskFunction <- NULL
  uID <- NULL
  logfilepaths <- c()
  
  # taskLog <- c()
  
  result <- NULL
  
  # to be used for the current running task
  taskHandle <- NULL
  curTask <- reactiveVal(0)
    
  # execute next task
  runNextTask <- function() {
    curTask(curTask() + 1)
    res <- FALSE
    
    # check that the next task is in limit
    if (curTask() <= length(tasks)) {
      # get task from list
      curTask <- tasks[[curTask()]]
      
      # add to log
      # taskLog <<- c(taskLog, names(tasks)[curTask()])
      
      # execute task
      taskHandle <<- curTask
      taskHandle$run()
      
      res <- TRUE
    }
    
    res
  }
  
  # run next task
  runNextTask()
  
  # kill process
  killProcess <- function() {
    taskHandle$kill()
  }
  
  # Poll every x milliseconds until the job completes
  o <- observe({
    # collect values
    res <- taskHandle$result()
    
    if (is.null(res)) {
      invalidateLater(poll)
    } else {
      # system process?
      retVal <- 0
      if (!is.null(attr(res[[1]], "status"))) {
        retVal <- attr(res[[1]], "status")
      }
      
      # hpc process?
      if (length(stringr::str_match(res[[1]], "slurm\\.fail")) > 0) {
        if (!any(is.na(stringr::str_match(res[[1]], "slurm\\.fail")))) {
          retVal <- 1
        }
      }
      
      # set parent variables
      if (taskResultSuccess(res) && retVal == 0) {
        state <<- "success"
        result <<- res[[1]]
        
        print(">> TASK SUCCESS")
        print(res)
      } else {
        print(">> TASK ERROR")
        print(res)
        
        state <<- "error"
        
        # system process?
        if (retVal > 0) {
          result <<- retVal
        } else {
          # result <<- attr(res[[1]], "condition", exact = TRUE)
          result <<- attr(res[[1]], "condition")
        }
        
        stopTaskExecution()
      }
      
      # run next task if there is one
      # otherwise, return result
      if (runNextTask() == FALSE) {
        # destroy if result is returned
        o$destroy()

        # # set log
        # taskLog <<- c(taskLog, sprintf("Result: %s", state))
      }
    }
  })
  
  # stop task execution
  stopTaskExecution <- function() {
    # stop task execution
    curTask(length(tasks) + 1)
  }
  
  # return state functions
  list(
    setTaskEnvironment = function(x) {
      taskEnvironment <<- x
    },
    setUID = function(x) {
      uID <<- x
    },
    setTaskID = function(x) {
      taskID <<- x
    },
    setTaskFunction = function(x) {
      taskFunction <<- x
    },
    setLogfilepaths = function(x) {
      logfilepaths <<- x
    },
    uID = function(x) {
      uID
    },
    taskID = function(x, fullID = TRUE) {
      if (fullID == TRUE) {
        taskID
      } else {
        stringr::str_extract(taskID, "(?<=\\.)[:alnum:]+$")
      }
    },
    taskFunction = function(x) {
      taskFunction
    },
    tasks = function(x) {
      tasks
    },
    getTaskEnvironment = function() {
      taskEnvironment
    },
    logfilepaths = function() {
      logfilepaths
    },
    taskHandle = function() {
      taskHandle
    },
    completed = function() {
      state != "running" && curTask() > length(tasks)
    },
    getState = function() {
      state
    },
    result = function() {
      if (state == "running") {
        # If running, abort the current context silently.
        # We've taken a reactive dependency on "state" so if
        # the state changes the context will invalidate.
        req(FALSE)
      } else if (state == "success") {
        return(result)
      } else if (state == "error") {
        return(result)
      } else if (state == "cancel") {
        validate(need(FALSE, "The operation was cancelled"))
      }
    },
    cancel = function() {
      o$destroy()
      killProcess()
      
      stopTaskExecution()
      state <<- "cancel"
    }
  )
}
