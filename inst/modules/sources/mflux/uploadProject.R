UploadProject <- R6::R6Class(
  "UploadProject",
  inherit = Mflux,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "uploadProject",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # upload whole project folder
      self$upload(self$globalParams()$projectsDir)
    }
  )
)
