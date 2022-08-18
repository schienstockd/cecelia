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
    initialize = function(connectionFile = NULL, useConnectionFile = FALSE) {
      # check config
      if (is.null(connectionFile) && useConnectionFile == TRUE)
        connectionFile <- file.path(
          cciaConf()$python$viewer$viewerPath,
          cciaConf()$python$viewer$connectionFile)
      
      jupyterClient <- reticulate::import("jupyter_client")
      
      # use connection file
      if (!is.null(connectionFile)) {
        message(paste(">> Use connection file", connectionFile))
        
        km <- jupyterClient$BlockingKernelClient(
          connection_file = connectionFile)
        km$load_connection_file()
        
        # set client and connection file
        private$setKernelClient(km)
        private$setKernelConnectionFile(connectionFile)
        
      } else {
        message(">> Start new Jupyter kernel")
        
        # otherwise start kernel
        jupyterKernel <- jupyterClient$manager$start_new_kernel()
        
        private$setKernelManager(jupyterKernel[[1]])
        private$setKernelClient(jupyterKernel[[2]])
        
        # set connection_file
        private$setKernelConnectionFile(
          self$kernelClient()$connection_file)
      }
      
      # set working directory to be safe
      self$execute(paste(
        "import os",
        sprintf("os.chdir('%s')", system.file(".", package = "cecelia")),
        sep = "\n"
      ))
      
      print(">> Jupyter kernel started")
      print(self$printConsoleConn())
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
      # TODO why does this not work on its own?
      self$execute("quit()")
      
      # shutdown
      if (!is.null(private$getKernelManager()))
        private$getKernelManager()$shutdown_kernel()
    },
    
    #' @description return kernel client
    kernelClient = function() {
      private$getKernelClient()
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
    
    ## getters
    getCondaEnv = function() {
      private$condaEnv
    },
    
    getKernelManager = function() {
      private$handleKernelManager
    },
    
    getKernelClient = function() {
      private$handleKernelClient
    }
  )
)