RunCciaSet <- R6::R6Class(
  "RunCciaSet",
  inherit = Launchpad,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "runCciaSet",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # launch task from object
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$initLog()
      self$writeLog("Launch tasks from set")
      
      # go through tasks and run
      for (i in names(self$funParams()$funTasks)) {
        x <- self$funParams()$funTasks[[i]]
        
        self$writeLog(sprintf(">> %s on %s", i, x$funEnv))
        
        # run task for object locally or remote on launchpad
        if (x$funEnv != "local") {
          # exchange running environment for local
          funParams <- self$funParams()
          
          # focus on current task
          funParams$funTasks <- funParams$funTasks[i]
          funParams$funTasks[[i]]$funEnv <- "local"
          
          # create new launchpad remote
          taskLauncher <- cciaObj$runTask(
            funName = "launchpad.runCciaSet",
            funParams = funParams,
            envVars = self$funParams()$envVars,
            hpcDir = if ("hpcDir" %in% names(self$funParams())) self$funParams()$hpcDir else NULL,
            env = x$funEnv,
            taskID = if ("taskID" %in% names(self$funParams())) self$funParams()$taskID else NULL,
            uploadSubmissionFiles = TRUE,
            runInplace = FALSE,
            pID = if ("pID" %in% names(self$funParams())) self$funParams()$pID else "000000",
            pName = if ("pName" %in% names(self$funParams())) self$funParams()$pName else "NONE"
          )
          
          # wait for result
          self$writeLog(paste(
            unlist(taskLauncher$result(wait = TRUE)), collapse = ''
          ))
        } else {
          taskLauncher <- cciaObj$runTasks(
            funName = i,
            funParams = x$funParams,
            envVars = self$funParams()$envVars,
            uIDs = self$funParams()$uIDs,
            hpcDir = if ("hpcDir" %in% names(self$funParams())) self$funParams()$hpcDir else NULL,
            env = x$funEnv,
            taskID = if ("taskID" %in% names(x)) x$taskID else NULL,
            uploadSubmissionFiles = TRUE,
            runInplace = TRUE,
            pID = if ("pID" %in% names(self$funParams())) self$funParams()$pID else "000000",
            pName = if ("pName" %in% names(self$funParams())) self$funParams()$pName else "NONE"
          )
        }
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
