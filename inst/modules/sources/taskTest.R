# Base class to test tasks
taskTest <- R6::R6Class(
  "taskTest",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "taskTest"
    }
  )
)