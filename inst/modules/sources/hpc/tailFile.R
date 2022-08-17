source(file.path(
  cfg$tasks$sources, "hpc.R")
)

TailFile <- R6::R6Class(
  "TailFile",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "tailFile",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # check and wait for file to be created
      self$sshConnection()$sshExecute(
        paste(
          # is file present?
          sprintf("test -f '%s'", self$funParams()$input),
          "||",
          # else watch
          sprintf("watch -g -t -n 0.1 'ls %s'", self$funParams()$input),
          sep = " "
        ), intern = TRUE)
      
      # tail file
      self$sshConnection()$sshExecute(
        paste(
          sprintf("tail -fn+1 %s", self$funParams()$input),
          sep = ";"
          ), intern = FALSE, outputFile = self$funParams()$output)
    }
  )
)