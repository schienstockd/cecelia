RetrieveCciaObj <- R6::R6Class(
  "RetrieveCciaObj",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "retrieveCciaObj",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$retrieve(
        basename(cciaObj$objFiles()),
        self$envParams("local")$dirs$task,
        self$envParams("hpc")$dirs$task
        )
    }
  )
)
