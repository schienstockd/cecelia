source(file.path(
  cfg$tasks$sources, "hpc.R")
)

UploadCciaObj <- R6::R6Class(
  "UploadCciaObj",
  inherit = Hpc,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "uploadCciaObj",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # upload
      self$upload(
        cciaObj$objFiles(),
        self$envParams("local")$dirs$task,
        self$envParams("hpc")$dirs$task
      )
      
      # upload uIDs for set?
      if ("uIDs" %in% names(self$funParams()) && "cciaObjects" %in% names(cciaObj)) {
        objFiles <- lapply(
          cciaObj$cciaObjects(uIDs = self$funParams()$uIDs), function(x) x$objFiles())

        # upload
        self$upload(
          unname(unlist(objFiles)),
          dirname(self$envParams("local")$dirs$task),
          dirname(self$envParams("hpc")$dirs$task)
        )
      }
    }
  )
)