# Base class to launch tasks
Launchpad <- R6::R6Class(
  "Launchpad",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "launchpad"
    }
  )
)