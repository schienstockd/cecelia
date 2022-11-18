# manage managers for modules
createModuleManager <- function(
  input, output, session, globalManagers, moduleName, managerNames, managerConf) {
  # list of managers
  managers <- reactiveVal()
  
  # classes that are not reactive
  nonReactiveManagers <- c("input", "ui")
  
  # init managers
  depCheck <- TRUE
  
  # check for manager dependencies
  if ("task" %in% managerNames) {
    if (any(!c("selection", "input") %in% managerNames)) {
      depCheck <- FALSE
    }
  }
  
  if ("imageViewer" %in% managerNames) {
    if (!("imageSet") %in% managerNames){
      depCheck <- FALSE
    }
  }     
  
  if (depCheck == TRUE) {
    # go through config and init managers
    managerList <- list()
    
    for (x in managerNames) {
      curVar <- sprintf("%sManager", x)
      
      # R6 classes
      if (x == "input") {
        curManager <- InputManager$new(
          input, session,
          file.path(cciaConf()$tasks$inputDefinitions, moduleName),
          globalManagers
          )
      } else if (x == "ui") {
        curManager <- UIManager$new(input, session)
      } else {
        # module-like managers
        # TODO have to check whether there is a better way to do this
        if (!x %in% nonReactiveManagers) {
          # init create function
          curCreateFun <- paste0(
            "create", cecelia:::firstToupper(x), "Manager")
          curCreateExpr <- sprintf(
            "%s(input, output, session, globalManagers, managers, managerConf)", curCreateFun
          )
          
          # assign manager
          curManager <- eval(parse(text = curCreateExpr))
        }
      }
      
      # add to managers
      managerList[[curVar]] <- curManager
    }
    
    # set reactive
    managers(managerList)
  }
  
  # return manager list
  managers
}
