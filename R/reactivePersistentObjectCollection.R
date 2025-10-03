#' Collection of reactive persistent object sets.
#' 
#' @name ReactivePersistentObjectCollection
#' @description Collection of persistent reactive object sets
#'
#' @examples
#' TODO
#' @export
ReactivePersistentObjectCollection <- R6::R6Class(
  "ReactivePersistentObjectCollection",
  inherit = ReactivePersistentObjectSet,
  
  ## private
  private = list(
    #' @description uID by Set name
    getUIDBySetName = function(setName) {
      uID <- NULL
      setNames <- self$cciaObjectNames()
      
      if (!is.null(setNames)){
        if (setName %in% setNames) {
          uID <- setNames[[which(setNames == setName)]]
        }
      }
      
      uID
    }
  ),
  
  ### public
  public = list(
    #' @description Add objects to set
    #' @param setName character of Set name
    #' @param objectsToAdd list of ReactivePersistentObjectSet 
    addCciaObjectsToSet = function(setName, objectsToAdd) {
      # get uID and object
      uID <- private$getUIDBySetName(setName)
      cciaObj <- cciaObjectByUID(uID)
      
      # add object to set
      cciaObj()$addCciaObjects(objectsToAdd)
    },
    
    #' @description Add new Set
    #' @param newSet ReactivePersistentObjectSet
    addCciaObjectSet = function(newSet) {
      # is name already given?
      if (is.null(private$getUIDBySetName(newSet()$getCciaName()))) {
        # add object to set
        self$addCciaObjects(newSet)
      }
    },
    
    #' @description Objects by Name
    #' @param setName character of Set name
    cciaObjectSetByName = function(setName) {
      # get uID
      uID <- private$getUIDBySetName(setName)
      
      self$cciaObjectByUID(uID)
    },
    
    #' @description ccia node object
    #' @param uID character of unique ID
    cciaNodeObjectByUID = function(uID) {
      obj <- NULL
      
      # go through all sets and find the uID
      for (curSet in self$cciaObjects()) {
        if (uID %in% names(curSet()$cciaObjects())) {
          obj <- curSet()$cciaObjects()[[uID]]
        }
      }
      
      obj
    }
  )
)
