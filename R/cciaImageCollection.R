#' Reactive image object collection
#' 
#' @name CciaImageCollection
#' @description Reactive image object collection
#'
#' @examples
#' TODO
#' @export
CciaImageCollection <- R6::R6Class(
  "CciaImageCollection",
  inherit = ReactivePersistentObjectCollection,
  
  ## private
  private = list(
  ),
  
  ### public
  public = list(
    #' @description Init
    #' @param stateFile character with file path to state file
    #' @param ... passed to super$initialize
    initialize = function(stateFile, ...) {
      super$initialize(stateFile = stateFile, ...)
      
      # create directories for processing
      if (length(stateFile) > 0) {
        for (x in cciaConf()$dirs$tasks[c("log", "tasks", "classifications")]) {
          dir.create(file.path(dirname(stateFile), x),
                     showWarnings = FALSE)
        }
      }
    },
    
    #' @description Classifier files
    #' @param clType character of classifier type
    #' @param fullPath boolean to return full path
    clFiles = function(clType, fullPath = FALSE) {
      # return entries of clType
      clFiles <- list.files(
        file.path(self$persistentObjectDirectory(),
                  cciaConf()$dirs$tasks$classifications, clType),
        full.names = fullPath
      )
      
      # set basenames as names
      if (fullPath == TRUE) {
        names(clFiles) <- basename(clFiles)
      }
      
      clFiles
    },
    
    #' @description Pixel classifier files
    #' @param ... passed to self$clFiles
    pixclFiles = function(...) {
      self$clFiles("pix", ...)
    }
  )
)
