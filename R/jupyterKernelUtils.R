#' Jupyter Kernel utils
#' 
#' @name JupyterKernelUtils
#' @description Jupyter Kernel utils
#'
#' @examples
#' TODO
#' @export
JupyterKernelUtils <- R6::R6Class(
  "JupyterKernelUtils",
  
  ## public
  public = list(
    #' @description Init
    #' @param condaEnv character for conda environment
    #' @param connectionFile character for connection file path
    initialize = function(connectionFile = NULL, useConnectionFile = TRUE,
                          libDir = NULL) {
      # check config
      if (is.null(connectionFile) && useConnectionFile == TRUE) {
        connectionFile <- system.file(file.path(
          "app",
          cciaConf()$python$viewer$viewerPath,
          cciaConf()$python$viewer$connectionFile), package = "cecelia")
      }
      
      # set working directory in jupyter
      if (is.null(libDir))
        libDir <- system.file(package = "cecelia")
      
      # use connection file
      if (!is.null(connectionFile)) {
        message(paste(">> Use connection file", connectionFile))
        
        jupyterClient <- reticulate::import("jupyter_client")
        
        km <- jupyterClient$BlockingKernelClient(connection_file = connectionFile)
        km$load_connection_file()
        
        # set client and connection file
        private$setKernelClient(km)
        private$setKernelConnectionFile(connectionFile)
        
        # set working directory to be safe
        self$execute(paste(
          "import os",
          sprintf("os.chdir('%s')", libDir),
          sep = "\n"
        ))
        
        message(">> Jupyter kernel started")
        message(self$printConsoleConn())
      } else {
        message(">> Start new Jupyter kernel process")
        
        # get connection file
        connectionFile <- system.file(file.path(
          "app",
          cciaConf()$python$viewer$viewerPath,
          cciaConf()$python$viewer$connectionFile), package = "cecelia")
        
        # file.remove(connectionFile)
        f <- file(connectionFile)
        writeLines(c(""), f)
        close(f)
        
        # start kernel in separate process
        private$setKernelProcess(
          parallel::mcparallel({
            # start kernel
            jupyterClient <- reticulate::import("jupyter_client")
            jupyterKernel <- jupyterClient$manager$start_new_kernel()
            
            # copy connection file
            file.copy(
              jupyterKernel[[2]]$connection_file,
              connectionFile,
              overwrite = TRUE)
          })
        )
        
        message(">> Wait for kernel")
        
        # wait until file is created
        f <- file(connectionFile)
        while (length(readLines(f)) <= 1) Sys.sleep(1/2)
        close(f)
        
        message(">> OK")
      }
    },
    
    #' @description print console connection info
    printConsoleConn = function() {
      sprintf(
        "jupyter-console --existing='%s'",
        self$getKernelConnectionFile()
      )
    },
    
    #' @description quit kernel
    quitKernel = function() {
      # send quit
      if (!is.null(self$kernelClient())) {
        # shutdown python kernel
        self$execute("exit()")
        
        # shutdown
        if (!is.null(private$getKernelManager()))
          private$getKernelManager()$shutdown_kernel()
      }
      
      # collect process
      if (!is.null(self$kernelProcess())) {
        parallel::mccollect(self$kernelProcess())
      }
    },
    
    #' @description return kernel client
    kernelClient = function() {
      private$getKernelClient()
    },
    
    #' @description return kernel process
    kernelProcess = function() {
      private$getKernelProcess()
    },
    
    #' @description execute command
    execute = function(cmd, silent = TRUE, execInteractive = TRUE) {
      if (execInteractive == TRUE) {
        self$kernelClient()$execute_interactive(
          cmd, silent = silent, store_history = FALSE)
      } else {
        self$kernelClient()$execute(
          cmd, silent = silent, store_history = FALSE)
      }
    },
    
    ## getters
    getKernelConnectionFile = function() {
      private$kernelConnectionFile
    }
  ),
  
  ## private
  private = list(
    condaEnv = NULL,
    kernelConnectionFile = NULL,
    handleKernelManager = NULL,
    handleKernelClient = NULL,
    handleKernelProcess = NULL,
    
    # # return kernel manager
    # kernelManager = function() {
    #   private$getKernelManager()
    # },
    
    ## setters
    setCondaEnv = function(x) {
      private$condaEnv <- x
    },
    
    setKernelConnectionFile = function(x) {
      private$kernelConnectionFile <- x
    },
    
    setKernelManager = function(x) {
      private$handleKernelManager <- x
    },
    
    setKernelClient = function(x) {
      private$handleKernelClient <- x
    },
    
    setKernelProcess = function(x) {
      private$handleKernelProcess <- x
    },
    
    ## getters
    getCondaEnv = function() {
      private$condaEnv
    },
    
    getKernelManager = function() {
      private$handleKernelManager
    },
    
    getKernelClient = function() {
      private$handleKernelClient
    },
    
    getKernelProcess = function() {
      private$handleKernelProcess
    }
  )
)