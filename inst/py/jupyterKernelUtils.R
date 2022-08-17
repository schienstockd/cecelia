popColourJupyterKernelUtils <- R6::R6Class(
  "JupyterKernelUtils",
  
  ## public
  public = list(
    ## init
    initialize = function(condaEnv = NULL, connectionFile = NULL) {
      # init conda environment
      if (!is.null(condaEnv)) {
        use_condaenv(condaEnv, required = TRUE)
      }
      
      jupyterClient <- import("jupyter_client")
      
      # use connection file
      if (!is.null(connectionFile)) {
        km <- jupyterClient$BlockingKernelClient(
          connection_file = connectionFile)
        km$load_connection_file()
        
        # set client and connection file
        private$setKernelClient(km)
        private$setKernelConnectionFile(connectionFile)
        
      } else {
        # otherwise start kernel
        jupyterKernel <- jupyterClient$manager$start_new_kernel()
        
        # private$setKernelManager(jupyterKernel[[1]])
        private$setKernelClient(jupyterKernel[[2]])
        
        # set connection_file
        private$setKernelConnectionFile(
          self$kernelClient()$connection_file)
      }
      
      print(">> Jupyter kernel started")
      print(self$printConsoleConn())
    },
    
    # print console connection info
    printConsoleConn = function() {
      sprintf(
        "jupyter-console --existing='%s'",
        self$getKernelConnectionFile()
      )
    },
    
    # quit kernel
    quitKernel = function() {
      # send quit
      self$execute("quit()")
      
      # shutdown
      # private$kernelManager()$shutdown_kernel()
    },
    
    # return kernel client
    kernelClient = function() {
      private$getKernelClient()
    },
    
    # execute command
    execute = function(cmd, silent = TRUE, execInteractive = TRUE) {
      if (execInteractive == TRUE) {
        self$kernelClient()$execute_interactive(
          cmd, silent = silent, store_history = FALSE)
      } else {
        self$kernelClient()$execute(
          cmd, silent = silent, store_history = FALSE)
      }
    },
    
    # getters
    getKernelConnectionFile = function() {
      private$kernelConnectionFile
    }
  ),
  
  ## private
  private = list(
    condaEnv = NULL,
    kernelConnectionFile = NULL,
    # handleKernelManager = NULL,
    handleKernelClient = NULL,
    
    # # return kernel manager
    # kernelManager = function() {
    #   private$getKernelManager()
    # },
    
    # setters
    setCondaEnv = function(x) {
      private$condaEnv <- x
    },
    
    setKernelConnectionFile = function(x) {
      private$kernelConnectionFile <- x
    },
    
    # setKernelManager = function(x) {
    #   private$handleKernelManager <- x
    # },
    
    setKernelClient = function(x) {
      private$handleKernelClient <- x
    },
    
    # getters
    getCondaEnv = function() {
      private$condaEnv
    },
    
    # getKernelManager = function() {
    #   private$handleKernelManager
    # },
    
    getKernelClient = function() {
      private$handleKernelClient
    }
  )
)