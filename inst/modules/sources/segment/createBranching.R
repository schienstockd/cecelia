CreateBranching <- R6::R6Class(
  "CreateBranching",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "createBranching",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$initLog()
      self$writeLog("Start Branching extraction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      valueName <- self$funParams()$valueName
      branchingName <- paste0(self$funParams()$valueName, ".", "branch")
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = cciaObj$imFilepath(),
        branchingName = branchingName,
        dilationSize = self$funParams()$dilationSize
      )
      
      # call python
      self$pyScript("create_branching", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      cciaObj$setImLabelsFilepath(
        paste0(branchingName, cciaConf()$files$ext$labels),
        valueName = branchingName
      )
      
      # load data from disk when loading the object
      # instead of pushing into object which makes it big
      # and slow to load
      cciaObj$setImLabelPropsFilepath(
        paste0(branchingName, cciaConf()$files$ext$labelProps),
        valueName = branchingName
      )
      
      # save object
      cciaObj$saveState()
    }
  )
)
