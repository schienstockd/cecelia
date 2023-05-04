#' Set of reactive persistent objects.
#' 
#' @name ReactivePersistentObjectSet
#' @description Set of persistent reactive objects
#'
#' @examples
#' TODO
#' @export
ReactivePersistentObjectSet <- R6::R6Class(
  "ReactivePersistentObjectSet",
  inherit = ReactivePersistentObject,
  
  ## private
  private = list(
    # object list
    cciaObjectList = NULL,
    
    #' @description Param from objects
    #' @param cciaObjects list of ReactivePersistentObject
    #' @param param character of param to get
    getParamOfCciaObjects = function(cciaObjects = NULL, param = "CciaName") {
      # get objects
      if (is.null(cciaObjects)) {
        cciaObjects <- self$cciaObjects()
      }
      
      params <- c()
      uIDs <- c()
      
      cciaObjects <- private$reqCciaObjectList(cciaObjects)
      
      if (length(cciaObjects) > 0) {
        for (curObj in cciaObjects) {
          # check reactivity
          if (private$initReactivity() == TRUE) {
            objParams <- curObj()[[sprintf("get%s", param)]]()
            objUID <- curObj()$getUID()
          } else {
            objParams <- curObj[[sprintf("get%s", param)]]()
            objUID <- curObj$getUID()
          }

          params <- c(params, objParams)
          uIDs <- c(uIDs, objUID)
        }

        # set names
        names(params) <- uIDs
      }
      
      params
    },
    
    #' @description Check that parameter is a object list
    #' @param cciaObjects list of ReactivePersistentObject
    reqCciaObjectList = function(cciaObjects) {
      if (typeof(cciaObjects) == "closure") {
        cciaObjects <- c(cciaObjects)
      }
      
      cciaObjects
    },
    
    ## setters
    setCciaObjects = function(x, invalidate = TRUE) {
      private$cciaObjectList <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setStateDir = function(x, invalidate = TRUE) {
      private$stateDir <- x
      private$invalidate(invalidate = invalidate)
    },
    
    ## getters
    getCciaObjects = function(uIDs = NULL) {
      if (is.null(private$cciaObjectList)) {
        return(list())
      }
      
      if (!is.null(uIDs)) {
        return(private$cciaObjectList[
          names(private$cciaObjectList) %in% uIDs])
      } else {
        return(private$cciaObjectList)
      }
    },
    
    getStateDir = function() {
      private$stateDir
    }
  ),
  
  ### public
  public = list(
    #' @description Add objects
    #' @param cciaObjects list of ReactivePersistentObject
    #' @param reset boolean to reset list
    #' @param invalidate boolean to invalidate object
    addCciaObjects = function(cciaObjects, reset = FALSE, invalidate = TRUE) {
      if (length(cciaObjects) > 0) {
        cciaObjects <- private$reqCciaObjectList(cciaObjects)
        
        # get uIDs
        names(cciaObjects) <- self$cciaObjectUIDs(cciaObjects)
        
        # overwrite objects that are already in the list
        newObjects <- list()
        if (reset == FALSE) {
          newObjects <- sapply(
            names(self$cciaObjects()),
            function(x) {
              if (!is.null(cciaObjects[[x]]))
                cciaObjects[[x]]
              else
                self$cciaObjects()[[x]]
              }
            )
        }
        
        # add to list
        cciaObjects <- append(
          newObjects,
          cciaObjects[!names(cciaObjects) %in% names(newObjects)]
          )
        
        # save back
        private$setCciaObjects(cciaObjects, invalidate = invalidate)
        
        private$invalidate(invalidate = invalidate)
      }
    },
    
    #' @description Remove objects
    #' @param cciaObjects list of ReactivePersistentObject
    #' @param removeContent boolean to remove object content
    #' @param invalidate boolean to invalidate object
    #' @param ... ReactivePersistentObject$deleteObjectDirectory
    removeCciaObjects = function(cciaObjects = NULL, removeContent = TRUE,
                                 invalidate = TRUE, ...) {
      if (is.null(cciaObjects)) {
        cciaObjects <- self$cciaObjects()
      }
      
      objectsToRemove <- self$cciaObjectUIDs(cciaObjects)
      
      # exclude uIDs
      newObjects <- self$cciaObjects()
      newObjects <- newObjects[!(names(newObjects) %in% objectsToRemove)]
      
      # remove content
      if (removeContent == TRUE && length(objectsToRemove) > 0) {
        for (curObj in self$cciaObjectsByUIDs(objectsToRemove)) {
          if (private$initReactivity() == TRUE)
            curObj()$deleteObjectDirectory(...)
          else
            curObj$deleteObjectDirectory(...)
        }
      }
      
      # set
      private$setCciaObjects(newObjects, invalidate = invalidate)
    },
    
    #' @description Remove object by uID
    #' @param uID character unique ID
    #' @param ... self$removeCciaObjects
    removeCciaObjectByUID = function(uID, ...) {
      self$removeCciaObjects(self$cciaObjectsByUIDs(uID), ...)
    },
    
    #' @description Attributes for objects
    #' @param cciaObjects list of ReactivePersistentObject
    cciaObjectsAttrNames = function(cciaObjects = NULL) {
      if (is.null(cciaObjects)) {
        cciaObjects <- self$cciaObjects()
      }
      
      objAttrNames <- c()
      
      # go through objects and get all attributes
      cciaObjects <- private$reqCciaObjectList(cciaObjects)
      
      if (length(cciaObjects) > 0) {
        for (curObj in cciaObjects) {
          objAttrNames <- c(
            objAttrNames,
            names(curObj()$getCciaAttr())
          )
        }
      }
      
      unique(objAttrNames)
    },
    
    #' @description Add attributes for objects
    #' @param objAttrName character for attribute name
    #' @param objAttrVals character for attribute value
    #' @param cciaObjects list of ReactivePersistentObject
    #' @param invalidate boolean to invalidate object
    addAttrForCciaObjects = function(objAttrName, objAttrVals = NULL,
                                     cciaObjects = NULL, invalidate = TRUE) {
      # get objects
      if (is.null(cciaObjects)) {
        cciaObjects <- self$cciaObjects()
      }
      
      cciaObjects <- private$reqCciaObjectList(cciaObjects)
      
      # generate empty values if none specified
      if (is.null(objAttrVals)) {
        objAttrVals <- rep("", length(cciaObjects))
        names(objAttrVals) <- names(cciaObjects)
        objAttrVals <- as.list(objAttrVals)
      }
      
      # go through objects
      if (length(cciaObjects) > 0) {
        for (curObjName in names(cciaObjects)) {
          if (private$initReactivity() == TRUE)
            cciaObjects[[curObjName]]()$addCciaAttr(objAttrName, objAttrVals[[curObjName]])
          else
            cciaObjects[[curObjName]]$addCciaAttr(objAttrName, objAttrVals[[curObjName]])
        }
      }
    },
    
    #' @description Edit attributes for objects
    #' @param objAttrName character for attribute name
    #' @param objAttrVals character for attribute value
    #' @param cciaObjects list of ReactivePersistentObject
    #' @param invalidate boolean to invalidate object
    editAttrForCciaObjects = function(objAttrName, objAttrVals, cciaObjects = NULL,
                                      invalidate = TRUE) {
      # get objects
      if (is.null(cciaObjects)) {
        cciaObjects <- self$cciaObjects()
      }
      
      cciaObjects <- private$reqCciaObjectList(cciaObjects)
      
      # go through objects
      if (length(cciaObjects) > 0) {
        for (curObjName in names(cciaObjects)) {
          cciaObjects[[curObjName]]()$editCciaAttr(
            objAttrName, objAttrVals[[curObjName]], invalidate = invalidate)
        }
      }
    },
    
    #' @description Delete attributes for objects
    #' @param objAttrName character for attribute name
    #' @param cciaObjects list of ReactivePersistentObject
    #' @param invalidate boolean to invalidate object
    delAttrForCciaObjects = function(objAttrName, cciaObjects = NULL,
                                     invalidate = TRUE) {
      # get objects
      if (is.null(cciaObjects)){
        cciaObjects <- self$cciaObjects()
      }
      
      cciaObjects <- private$reqCciaObjectList(cciaObjects)
      
      if (length(cciaObjects) > 0) {
        for (curObj in cciaObjects) {
          curObj()$delCciaAttr(objAttrName, invalidate = invalidate)
        }
      }
    },
    
    #' @description Save objects
    #' @param includeChildren boolean to include child nodes
    #' @param ... super$saveState & ReactivePersistentObject$saveState
    saveState = function(includeChildren = TRUE, ...) {
      super$saveState(...)
      
      # save child objects
      if (includeChildren == TRUE) {
        for (curObj in self$cciaObjects()) {
          if (private$initReactivity() == TRUE)
            curObj()$saveState(...)
          else
            curObj$saveState(...)
        }
      }
    },
    
    #' @description Load objects
    #' @param stateFiles list of state file paths
    #' @param initTransaction boolean to init transaction
    #' @param invalidate boolean to invalidate object
    #' @param includeChildren boolean to include child nodes
    retrieveState = function(stateFiles = NULL, initTransaction = FALSE,
                             invalidate = TRUE, includeChildren = FALSE) {
      if (!is.null(stateFiles) && length(stateFiles) > 0) {
        # load child objects
        loadedObjects <- list()
        
        for (curFile in stateFiles) {
          # create new objects
          if (private$initReactivity() == TRUE)
            curObj <- CciaObject$new(curFile, initTransaction = initTransaction)$reactive()
          else
            curObj <- CciaObject$new(curFile, initTransaction = initTransaction)
          
          loadedObjects <- append(loadedObjects, curObj)
        }
        
        # add to pool
        self$setCciaObjects(loadedObjects)
      }
      
      super$retrieveState(initTransaction = initTransaction, invalidate = invalidate)
      
      # already done in initialize of objects
      # retrieve state for all children
      if (includeChildren == TRUE) {
        for (cciaObj in self$cciaObjects()) {
          if (private$initReactivity() == TRUE)
            cciaObj()$retrieveState(initTransaction = initTransaction, invalidate = invalidate)
          else
            cciaObj$retrieveState(initTransaction = initTransaction, invalidate = invalidate)
        }
      }
    },
    
    #' @description Overview dataframe
    #' @param fields list of character field names
    #' @param withSelf boolean to include self
    #' @param trimAttr boolean to trim attribute names
    #' @param uIDs list of character unique IDs
    summary = function(fields = NULL, withSelf = TRUE, trimAttr = TRUE,
                       uIDs = NULL) {
      # go through all objects and assemble df
      objectSummaries <- list()
      
      # add yourself first
      if (withSelf == TRUE){
        objectSummaries[[self$getUID()]] <- list(
          "uID" = self$getUID(),
          "Name" = self$getCciaName()
        )
      }
      
      for (curObj in self$cciaObjects(uIDs = uIDs)) {
        if (private$initReactivity() == TRUE)
          objectSummaries[[curObj()$getUID()]] <- unlist(curObj()$summary(fields))
        else
          objectSummaries[[curObj$getUID()]] <- unlist(curObj$summary(fields))
      }
      
      # attributes might be missing
      # replace with NA
      # https://stackoverflow.com/a/17309310/13766165
      # summaryDF <- as.data.frame(do.call(rbind, objectSummaries))
      summaryDF <- plyr::rbind.fill(
        lapply(objectSummaries, function(y) {
          as.data.frame(t(y), stringsAsFactors = FALSE)
          })
        )
      
      if (!is.null(summaryDF)) {
        # remove name from attributes
        names(summaryDF) <- gsub(
          x = names(summaryDF), pattern = "Attr\\.", replacement = "") 
      }
      
      # as.data.table(summaryDF)
      summaryDF
    },
    
    #' @description Run tasks for children
    #' @param uIDs list of character of unique IDs
    #' @param ... ReactivePersistentObject$runTask
    runTasks = function(uIDs = NULL, mc.cores = NULL, ...) {
      # set task number
      if (is.null(mc.cores))
        mc.cores <- parallel::detectCores()
      
      # set IDs
      if (is.null(uIDs)) {
        uIDs <- names(self$cciaObjects())
      }
      
      # go through children
      # for (x in self$cciaObjects()[uIDs]) {
      parallel::mclapply(self$cciaObjects()[uIDs], function(x) {
        .cciaMessageParallel(sprintf(">> Run task for %s", x$getUID()))
        x$runTask(...)
        }, mc.cores = mc.cores)
    },
    
    #' @description All objects
    #' @param uIDs list of character of unique IDs
    cciaObjects = function(uIDs = NULL) {
      if (is.null(private$cciaObjectList)) {
        return(list())
      }
      
      if (!is.null(uIDs)) {
        return(private$cciaObjectList[
          names(private$cciaObjectList) %in% uIDs])
      } else {
        return(private$cciaObjectList)
      }
    },
    
    #' @description Objects by uID
    #' @param uIDs list of character of unique IDs
    cciaObjectsByUIDs = function(uIDs) {
      private$cciaObjectList[
        names(private$cciaObjectList) %in% uIDs]
    },
    
    cciaObjectByUID = function(uID) {
      self$cciaObjectsByUIDs(c(uID))
    },
    
    #' @description Objects by attribute
    #' @param attrName character of attribute name
    #' @param attrValue character/numeric of attribute value
    cciaObjectsByAttr = function(attrName, attrValue) {
      cciaObjs <- lapply(self$cciaObjects(), function(x) {
        if (x$getCciaAttr(attrName) == attrValue)
          x
      })
      
      # return non null elements
      cciaObjs[!sapply(cciaObjs, is.null)]
    },
    
    ## setters
    setCciaObjectUIDs = function(uIDs, invalidate = TRUE, reset = FALSE) {
      objDir <- self$persistentObjectDirectory(root = TRUE)
      objsToAdd <- list()
      
      # load objects from directory
      for (uID in uIDs) {
        # create class
        objsToAdd[[uID]] <- initCciaObject(
          file.path(objDir, uID),
          initReactivity = private$initReactivity(),
          initTransaction = self$isTransaction()
          )
      }
      
      # If objects are removed and added during the shiny session
      # and the object is reloaed, the removed
      # objects would still be in the list
      self$addCciaObjects(objsToAdd, reset = TRUE)
    },
    
    ## getters
    getCciaObjectUIDs = function() {
      self$cciaObjectUIDs()
    },
    
    cciaObjectUIDs = function(cciaObjects = NULL) {
      private$getParamOfCciaObjects(
        cciaObjects, "UID"
      )
    },
    
    cciaObjectNames = function(cciaObjects = NULL) {
      private$getParamOfCciaObjects(
        cciaObjects, "CciaName"
      )
    },
    
    cciaObjectMetas = function(cciaObjects = NULL) {
      private$getParamOfCciaObjects(
        cciaObjects, "CciaMeta"
      )
    }
  )
)
