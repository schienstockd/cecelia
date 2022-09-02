#' Task Launcher will return a TaksProcess object for execution
#' 
#' @name TaskLauncher
#' @description
#' Very naive task processing framework. This should be replaced with:
#' https://github.com/mschubert/clustermq
#'
#' @examples
#' TODO
#' @export
TaskLauncher <- R6::R6Class(
  "TaskLauncher",
  inherit = TaskCore,
  
  private = list(
    taskFunction = NULL,
    taskFunctionClass = NULL,
    inplace = FALSE,
    
    # unique id for task
    taskID = NULL,
    
    # process handling
    taskHandle = NULL,
    
    # HPC specifics
    hpcConf = list(),
    
    # Sub HPC tasks
    subHPCJobs = c(),
    
    #' @description Run task inplace
    runInplaceTask = function() {
      private$runLocalTask()
    },
    
    #' @description Run local task
    runLocalTask = function() {
      # convert function name to directory
      funPath <- stringr::str_split(
        self$getTaskFunction(), paste0("\\", CCID_CLASS_SEP))[[1]]
        
      parentClassPath <- funPath[[1]]
      moduleClassPath <- paste(funPath, collapse = .Platform$file.sep)
      
      # get source file
      parentSourceFile <- file.path(
        system.file(cciaConf()$tasks$sources, package = "cecelia"),
        paste(parentClassPath, "R", sep = "."))
      moduleSourceFile <- file.path(
        system.file(cciaConf()$tasks$sources, package = "cecelia"),
        paste(moduleClassPath, "R", sep = "."))
      
      cmd <- sprintf(paste(
        "parentSourceFile <- \"%s\"",
        "moduleSourceFile <- \"%s\"",
        "source(parentSourceFile, local = TRUE)",
        "source(moduleSourceFile, local = TRUE)",
        # init object
        "taskProcess <- %s$new()",
        # init param file
        "taskProcess$prepRun(\"%s\")",
        # sink divert
        # TODO that does not work well
        # "sink(\"%s\")",
        # run task
        "taskProcess$run()",
        # detach sink
        # "sink()",
        sep = "\n"
        ),
        parentSourceFile,
        moduleSourceFile,
        private$getTaskFunctionClass(),
        self$taskInputFile()
        # self$taskOutputFile()
        )
      
      cmd
    },
    
    #' @description Prep hpc task
    #' @param uploadSubmissionFiles boolean to upload submission files
    prepHPCTask = function(uploadSubmissionFiles = TRUE) {
      # create job name
      curJobName <- sprintf(
        "%s (%s): %s.%s",
        self$globalParams()$pName,
        self$getTaskFunction(),
        self$globalParams()$uID,
        self$getTaskID()
      )
      
      # watch exit only?
      if (!"exitOnly" %in% names(self$envParams()$conf) ||
        self$envParams()$conf$exitOnly == FALSE) {
        # build email alerts
        emailAlerts <- c()
        
        if (self$envParams()$conf$emailOnBegin == TRUE) {
          emailAlerts <- c(emailAlerts, "#SBATCH --mail-type=BEGIN")
        }
        if (self$envParams()$conf$emailOnEnd == TRUE) {
          emailAlerts <- c(emailAlerts, "#SBATCH --mail-type=END")
        }
        if (self$envParams()$conf$emailOnFail == TRUE) {
          emailAlerts <- c(emailAlerts, "#SBATCH --mail-type=FAIL")
        }
  
        emailAlerts <- paste(emailAlerts, collapse = "\n")
        
        # build job script and push to file
        slurmParams <- list(
          CCIA_nNODES = self$envParams()$conf$numNodes,
          CCIA_nTASKS = self$envParams()$conf$numTasks,
          CCIA_CPU_PER_TASK = self$envParams()$conf$numCPUperTask,
          CCIA_GPU_PER_TASK = self$envParams()$conf$numGPUperTask,
          CCIA_MEMORY = self$envParams()$conf$memory * 1024,
          CCIA_WALLTIME = self$envParams()$conf$walltime,
          
          CCIA_PARTITIONS = self$envParams()$conf$projectPartitions,
          CCIA_PROJECT_ACCOUNT = self$envParams()$conf$projectID,
          
          CCIA_JOB_NAME = curJobName,
          CCIA_EMAIL_ALERTS = emailAlerts,
          CCIA_EMAIL_ADDRESS = self$envParams()$conf$email,
          CCIA_JOB_COMMAND = paste(
            # change into working directory
            # sprintf("cd %s", self$envParams()$dirs$task),
            # change into cecelia directory
            # sprintf("cd %s", cciaConf()$hpc$dirs$cecelia),
            # run task launcher
            #   "Rscript %s/managers/tasks/taskLauncher.R --inplace --fun \"%s\" --conf \"%s\"",
            #   cciaConf()$hpc$dirs$cecelia,
            #   self$getTaskFunction(),
            #   self$taskInputFile()
            paste0("Rscript -e '", paste(
              "library(cecelia)",
              paste0("cciaUse(\"", cciaConf()$hpc$dirs$cecelia, "\")"),
              "taskLauncher <- TaskLauncher$new()",
              paste0(
                "taskLauncher$initTask(\"", self$getTaskFunction(),
                "\", taskConfFile = \"", self$taskInputFile(), "\", inplace = TRUE)"
                ),
              "taskLauncher$run()",
              sep = ";"
              ), "'"),
            private$hpcCompletionPostfix(
              self$getHPCConf("hpcSlurmFile")
            ),
            sep = "\n"
          )
        )
        
        # read template file
        if (self$envParams()$conf$useGPU == TRUE) {
          curTplFile <- taskMANAGER_HPC_SLURM_GPU_TPL
        } else {
          curTplFile <- taskMANAGER_HPC_SLURM_CPU_TPL
        }
        
        curSlurm <- readLines(system.file(
          curTplFile, package = "cecelia"))
        
        # go through params and replace
        for (i in names(slurmParams)) {
          curSlurm <- gsub(
            pattern = sprintf("\\$%s", i),
            replace = slurmParams[[i]],
            x = curSlurm)
        }
        
        # write slurm params
        writeLines(curSlurm, self$getHPCConf("localSlurmFile"))
        
        # upload submission files
        if (uploadSubmissionFiles == TRUE) {
          # create sshUtil
          sshUtil <- private$sshUtils()
          
          submissionFiles <- c(
            self$taskInputFile("local"),
            self$getHPCConf("localSlurmFile")
          )
        
          # sync JSON config and slurm file to HPC
          sshUtil$syncRemoteFiles(
            submissionFiles,
            self$envParams("local")$dirs$task,
            self$envParams("hpc")$dirs$task,
            silent = TRUE
          )
          
          # remove files locally
          unlink(submissionFiles)
        }
        
        # prepare task config and add calling module
        taskConf <- private$getTaskConf()
        taskConf$fun$callingFun <- .trimModuleFunName(
          self$getTaskFunction())
        
        # reset image information before running task
        self$runTasks(
          private$parentModule(
            self$getTaskFunction(),
            "resetImageInfo"
            ),
          taskConf = taskConf,
          runLocal = TRUE)
      }
    },

    #' @description Run HPC task
    #' @param exitOnly boolean to wait for exit only
    runHPCTask = function(exitOnly = FALSE) {
      # run job submission
      cmd <- sprintf("cd %s", self$envParams()$dirs$task)
      
      # prepare job id file
      jobIDFile <- sprintf("%s.job_id", self$getHPCConf("hpcSlurmFile"))
      
      runCmd <- paste(
        sprintf("yes | rm -f %s.*", self$getHPCConf("hpcSlurmFile")),
        # https://stackoverflow.com/questions/49110967/replace-multiple-variables-in-sprintf-with-same-value#49111019
        sprintf(
          "sbatch -o %1$s.log -e %1$s.log %1$s | tr -cd [:digit:] > %1$s.job_id",
          self$getHPCConf("hpcSlurmFile")
        ),
        sep = "; "
      )
      
      if (exitOnly == FALSE) {
        cmd <- paste(cmd, runCmd, sep = "; ")
      }
      
      # add watch exit
      cmd <- paste(
        cmd, private$hpcWatchJobExit(cmdOnly = TRUE),
        # remove job id file after finish
        sprintf("rm %s", jobIDFile),
        sep = "; ")
      
      # execute
      private$sshUtils()$sshExecute(cmd, wrapWithSystem = TRUE)
    },
    
    #' @description Add HPC task completion postfix
    #' @param filename character for filename
    hpcCompletionPostfix = function(filename) {
      paste(
        "if [ $? -eq 0 ]",
        "then",
        sprintf("touch %s.success", filename),
        "else",
        sprintf("touch %s.fail", filename),
        "fi",
        sep = "\n"
      )
    },
    
    #' @description Wait for HPC job completion
    #' @param cmdOnly boolean return command only
    hpcWatchJobExit = function(cmdOnly = FALSE) {
      cmd <- paste(
        "sh",
        paste(
          cciaConf()$hpc$dirs$scripts,
          cciaConf()$hpc$scripts$watchJobCompletion,
          sep = "/"
        ),
        self$getHPCConf("hpcSlurmFile")
      )
      
      # execute
      if (cmdOnly == FALSE) {
        handleSystem(private$sshUtils()$sshExecute(cmd))
      } else {
        cmd
      }
    },
    
    #' @description
    #' Cancel hpc job; https://stackoverflow.com/a/7547391/13766165
    #' @param jobsToCancel list of character of jobs to cancel
    killHPCJobs = function(jobsToCancel = NULL) {
      # get jobs to cancel
      if (is.null(jobsToCancel))
        jobsToCancel <- self$getHPCConf("hpcSlurmFile")
      
      # prepare cancel
      cmd <- sprintf(
        "xargs -a %s.job_id scancel",
        jobsToCancel
      )
      
      # execute
      handleSystem(private$sshUtils()$sshExecute(cmd), silent = TRUE)
    },
    
    #' @description Get job result
    #' @param readLog boolean to return log content
    hpcJobLog = function(readLog = FALSE) {
      private$sshUtils()$syncLocalFiles(
        file.path(self$taskLogFile(local = FALSE)),
        self$envParams()$dirs$task,
        self$envParams("local")$dirs$task,
        createDir = FALSE)
      
      if (readLog == TRUE) {
        read_lines(self$taskLogFile())
      }
    },
    
    #' @description Get ssh util
    sshUtils = function() {
      SshUtils$new(
        username = self$utilsParams()$ssh$username,
        address = self$utilsParams()$ssh$address,
        keyfile = self$utilsParams()$ssh$keyfile
      )
    },
    
    ## setters
    setInplace = function(x) {
      private$inplace <- x
    },
    
    setTaskFunction = function(x) {
      private$taskFunction <- x
      
      # prepare class
      className <- strsplit(x, paste0("\\", CCID_CLASS_SEP))[[1]]
      className <- className[length(className)]
      className <- firstToupper(className)
      
      # set class
      private$setTaskFunctionClass(className)
    },
    
    setTaskFunctionClass = function(x) {
      private$taskFunctionClass <- x
    },
    
    setTaskHandle = function(x) {
      private$taskHandle <- x
    },
    
    setTaskConfFile = function(x) {
      # private$taskConfFile <- x
      # get task ID from config file
      taskID <- basename(stringr::str_replace(x, ".input.json", ""))
      taskID <- stringr::str_split(taskID, "\\.")
      taskID <- taskID[[1]][length(taskID[[1]])]
      
      # set id
      private$setTaskID(taskID)
    },
    
    setTaskID = function(x) {
      private$taskID <- x
    },
    
    ## getters
    getInplace = function() {
      private$inplace
    },
    
    getTaskFunctionClass = function() {
      private$taskFunctionClass
    },
    
    getTaskHandle = function() {
      private$taskHandle
    },
    
    getTaskConfFile = function() {
      private$taskConfFile
    }
  ),
  
  public = list(
    #' @description Init
    initialize = function() {
    },
    
    #' @description Init task
    #' @param taskFunction character for task function
    #' @param taskConf list of list of character for task config
    #' @param taskConfFile character for task config file
    #' @param inplace boolean to run inplace
    #' @param cciaObj ReactivePersistentObject to use cached data
    #' @param taskID integer for task ID
    #' @param forceHPCConf boolean to prepare HPC files
    initTask = function(
      taskFunction, taskConf = NULL, taskConfFile = NULL, inplace = FALSE,
      cciaObj = NULL, taskID = NULL, forceHPCConf = FALSE) {
      # set task function
      private$setTaskFunction(taskFunction)
      
      # TODO set ccia obj for in place tasks
      # then the object could use cached data
      # .. would that be good actually?
      # if data is changed anywhere else this
      # would not be recognised
      
      # set whether to run in place
      private$setInplace(inplace)
      
      # load config from file
      if (!is.null(taskConfFile)) {
        private$setTaskConfFile(taskConfFile)
        taskConf <- jsonlite::fromJSON(readLines(taskConfFile))
      } else {
        if (is.null(taskID)) {
          taskID <- sample(1:10, 1)
        }
        
        # generate new id for config
        private$setTaskID(taskID)
      }
      
      # init parameters
      private$setTaskConf(taskConf)
      
      # return ID
      self$getTaskID()
      
      # prepare hpc files
      if (self$taskEnv() == "hpc" || forceHPCConf == TRUE) {
        slurmFilename <- sprintf("%s.%s.slurm",
                                 self$getTaskFunction(),
                                 self$getTaskID())
        
        # prepare slurm files
        self$setHPCConf(
          "hpcSlurmFile",
          paste(
            self$envParams("hpc")$dirs$task,
            cciaConf()$dirs$tasks$tasks,
            slurmFilename,
            sep = "/"
          )
        )
        self$setHPCConf(
          "localSlurmFile",
          paste(
            self$envParams("local")$dirs$task,
            cciaConf()$dirs$tasks$tasks,
            slurmFilename,
            sep = "/"
          )
        )
      }
    },
    
    #' @description Get input file for task
    #' @param taskEnv character for task environment
    taskInputFile = function(taskEnv = NULL) {
      if (is.null(taskEnv)) {
        taskEnv <- self$taskEnv()
      }
      
      file.path(
        self$envParams(taskEnv)$dirs$task,
        cciaConf()$dirs$tasks$tasks,
        paste(
          self$getTaskFunction(),
          self$getTaskID(),
          "input.json",
          sep = CCID_CLASS_SEP)
      )
    },
    
    #' @description Get output file for task
    #' @param local boolean to return local file
    taskOutputFile = function(local = TRUE) {
      if (local == TRUE) {
        taskEnv <- "local"
      } else {
        taskEnv <- self$taskEnv()
      }
      
      file.path(
        self$envParams(taskEnv)$dirs$task,
        cciaConf()$dirs$tasks$tasks,
        paste(
          self$getTaskFunction(),
          self$getTaskID(),
          "system.log",
          sep = CCID_CLASS_SEP)
      )
    },
    
    #' @description Clear task log file
    #' @param ... passed to self$taskLogFile
    clearTaskLogFile = function(...) {
      # writeLines("", self$taskLogFile(...))
      unlink(self$taskLogFile(...))
    },
    
    #' @description Get task log file
    #' @param fullPath boolean to return full path
    #' @param local boolean to return local file
    taskLogFile = function(fullPath = TRUE, local = TRUE) {
      logFile <- paste(self$getTaskFunction(), self$getTaskID(), "log", sep = ".")
      
      if (fullPath == TRUE) {
        if (local == TRUE) {
          logFile <- file.path(
            self$envParams("local")$dirs$task,
            cciaConf()$dirs$tasks$log,
            logFile
          )
        } else {
          logFile <- file.path(
            self$envParams()$dirs$task,
            cciaConf()$dirs$tasks$log,
            logFile
          )
        }
      }
      
      logFile
    },
    
    #' @description Prepare for run
    #' @param uploadSubmissionFiles boolean to upload submission files
    #' @param remoteAsLocal boolean to set remote location as local
    prepRun = function(uploadSubmissionFiles = TRUE, remoteAsLocal = FALSE) {
      taskConf <- private$getTaskConf(remoteAsLocal = remoteAsLocal)
      
      # add calling function if not defined
      if (!"callingFun" %in% names(taskConf$fun)) {
        taskConf$fun$callingFun <- .trimModuleFunName(
          self$getTaskFunction())
      }
      
      # add task ID
      taskConf$env$global$taskID <- self$getTaskID()
      
      # save as combined JSON
      exportJSON <- jsonlite::toJSON(taskConf)
      
      # save in task directory
      write(
        exportJSON, self$taskInputFile("local")
      )
      
      # run other preparations if necessary
      if (self$taskEnv() == "hpc") {
        private$prepHPCTask(uploadSubmissionFiles = uploadSubmissionFiles)
      }
    },
    
    #' @description Run task
    run = function() {
      # the logic depends on the run environment
      if (private$getInplace() == TRUE) {
        cmd <- parse(text = private$runInplaceTask())
        
        eval(cmd)
      } else {
        if (self$taskEnv() == "local") {
          cmd <- private$runLocalTask()
        } else if (self$taskEnv() == "hpc") {
          # watch exit only?
          exitOnly <- FALSE
          if ("exitOnly" %in% names(self$envParams("hpc")$conf))
            exitOnly <- self$envParams("hpc")$conf$exitOnly
          
          cmd <- private$runHPCTask(exitOnly = exitOnly)
        }
        
        # parse command
        cmd <- parse(text = cmd)
        
        print(">> EXEC")
        print(cmd)
        
        # run command
        private$setTaskHandle(
          parallel::mcparallel({
            force(cmd)
          })
        )
      }
    },
    
    #' @description Kill task
    kill = function() {
      if (!is.null(private$getTaskHandle())) {
        # kill hpc job
        if (self$taskEnv() == "hpc") {
          private$killHPCJobs()
        }
        
        # kill sub HPC jobs
        if (length(self$getSubHPCJobs()) > 0) {
          private$killHPCJobs(self$getSubHPCJobs())
        }
        
        # get all children of process
        # otherwise a python process called from this task
        # would continue running even if the parent is killed
        # TODO does that still work..?
        pidTree <- .execSystem(
          sprintf("pstree -s -p %s", private$getTaskHandle()$pid))
        pids <- as.numeric(stringr::str_match(pidTree, "(?<= )[0-9]+(?= )"))
        
        # go through all processes and cancel
        for (curPID in pids) {
          tools::pskill(curPID, tools::SIGTERM)
          tools::pskill(-curPID, tools::SIGTERM)
        }
        
        # handle mcparallel
        parallel::mccollect(private$getTaskHandle(), wait = FALSE)
      }
    },
    
    #' @description Check whether task has finished
    #' @param wait boolean to wait for job exit
    result = function(wait = FALSE) {
      procRes <- parallel::mccollect(private$getTaskHandle(), wait = wait)
      
      # handle hpc
      if (wait == TRUE && self$taskEnv() == "hpc") {
        private$hpcWatchJobExit()
      }
      
      procRes
    },
    
    #' @description Return task log
    #' @param locationOnly character to return location only
    #' @param ... passed to self$taskLogFile
    taskLog = function(locationOnly = FALSE, ...) {
      retVal <- NULL
      
      # something like a sink from stdout
      # create a tempfile and then tell the process to write into that
      # that would need to be set in TaskProcessor when creating the expression
      # https://stackoverflow.com/a/27444424/13766165
      
      # # copy log from remote
      # if (self$taskEnv() == "hpc") {
      #   private$hpcJobLog()
      # }
      
      # return content of logfile?
      # or just the path and the calling object
      # can use reactiveFileReader to get the content
      if (locationOnly == TRUE) {
        retVal <- self$taskLogFile(...)
      } else {
        retVal <- read_file(self$taskLogFile(...))
      }
      
      retVal
    },
    
    ## setters
    setHPCConf = function(key, x) {
      private$hpcConf[[key]] <- x
    },
    
    setSubHPCJobs = function(x) {
      private$subHPCJobs <- x
    },
    
    ## getters
    getHPCConf = function(key) {
      private$hpcConf[[key]]
    },
    
    getSubHPCJobs = function() {
      private$subHPCJobs
    },
    
    getTaskFunction = function() {
      private$taskFunction
    },
    
    getTaskID = function() {
      private$taskID
    }
  )
)

###
# Run launcher from command line
# https://community.rstudio.com/t/command-line-argument-parser-for-rscript/7124/5
# https://gist.github.com/ericminikel/8428297
# https://github.com/trevorld/r-optparse/blob/b6306a1613b3743719a9dc436dcb808dbb332891/R/optparse.R
###
# https://stackoverflow.com/a/47178017/13766165
if (sys.nframe() == 0) {
  option_list = list(
    optparse::make_option(c("-i", "--inplace"), action = "store_true", default = FALSE,
                help = "Run command inplace"),
    optparse::make_option(c("-p", "--prepRun"), action = "store_true", default = FALSE,
                help = "Run prep before function run"),
    optparse::make_option(c("-f", "--fun"), action = "store", default = NULL,
                type = 'character', help = "Function to run"),
    optparse::make_option(c("-c", "--conf"), action = "store", default = NULL,
                type = 'character', help = "JSON config file")
  )
  opt = optparse::parse_args(OptionParser(option_list = option_list))
  
  # run command
  # init task launcher
  taskLauncher <- TaskLauncher$new()
  
  taskLauncher$initTask(
    opt$fun, taskConfFile = opt$conf, inplace = opt$inplace)
  
  # prep parameters?
  # otherwise it is assumed that they are already in place
  if (opt$prepRun == TRUE) {
    taskLauncher$prepRun()
  }
  
  # run task
  taskLauncher$run()
}
