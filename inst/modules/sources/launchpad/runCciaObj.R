RunCciaObj <- R6::R6Class(
  "RunCciaObj",
  inherit = Launchpad,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "runCciaObj",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # launch task from object
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$initLog()
      self$writeLog("Launch tasks")
      
      # go through tasks and run
      for (i in names(self$funParams()$funTasks)) {
        x <- self$funParams()$funTasks[[i]]
        
        self$writeLog(sprintf(">> %s on %s", i, x$funEnv))
        self$writeLog("---")
        
        # run task for object
        taskLauncher <- cciaObj$runTask(
          funName = i,
          funParams = x$funParams,
          envVars = self$funParams()$envVars,
          hpcDir = if ("hpcDir" %in% names(self$funParams())) self$funParams()$hpcDir else NULL,
          env = x$funEnv,
          taskID = if ("taskID" %in% names(x)) x$taskID else NULL,
          uploadSubmissionFiles = TRUE,
          runInplace = FALSE,
          pID = if ("pID" %in% names(self$funParams())) self$funParams()$pID else "000000",
          pName = if ("pName" %in% names(self$funParams())) self$funParams()$pName else "NONE"
        )
        
        # wait for result
        taskResult <- taskLauncher$result(wait = TRUE)
        
        sapply(taskResult, function(x) self$writeLog(as.character(x)))
        
        if (!taskResultSuccess(taskResult))
          stop(taskResult)
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
