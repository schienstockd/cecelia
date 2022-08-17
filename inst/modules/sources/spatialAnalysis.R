# Base class for gating methods
SpatialAnalysis <- R6::R6Class(
  "SpatialAnalysis",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "spatialAnalysis"
    },
    
    # get regions filename
    regionsFilename = function() {
      paste0(
        "regions_",
        self$funParams()$valueName,
        cfg$files$ext$labelProps)
    },
    
    # reset image information
    resetImageInfo = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get calling function
      if (self$funParams()$callingFun == "cellRegions") {
        if (is.null(cciaObj$imRegionsFilepath(valueName = self$funParams()$valueName))) {
          cciaObj$setImRegionsFilepath(
            "SPACEHOLDER",
            valueName = self$funParams()$valueName
          )
        }
      }

      # save object
      cciaObj$saveState()
    },
    
    # update image information
    updateImageInfo = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get calling function
      if (self$funParams()$callingFun == "cellRegions") {
        cciaObj$setImRegionsFilepath(
          self$regionsFilename(),
          valueName = self$funParams()$valueName
        )
      } else if (self$funParams()$callingFun %in% c("cellClusters", "cellClustersMeshes")) {
        # add populations to popMap
        for (i in self$funParams()$popsToCluster) {
          popType <- self$funParams()$popType
          parentPops <- c(i)
          pops <- list(
            "clustered" = list(
              filterMeasure = sprintf(
                "%s.cell.is.clust", self$funParams()$popType),
              filterValues = TRUE,
              filterFun = "eq"
            ),
            "non.clustered" = list(
              filterMeasure = sprintf(
                "%s.cell.is.clust", self$funParams()$popType),
              filterValues = FALSE,
              filterFun = "eq",
              filterDefaultAll = TRUE
            )
          )
          
          # remove populations
          cciaObj$delPopsByPath(
            popType,
            pops = levels(interaction(parentPops, names(pops), sep = "/")),
            includeFiltered = TRUE
          )
          
          # get value name from parent
          popValueName <- cciaObj$popAttr(
            self$funParams()$popType,
            "valueName", popPath = i, includeFiltered = TRUE)[[1]]
          
          # add populations
          cciaObj$addFilteredPops(popType, parentPops, pops,
                                  valueName = popValueName)
          
          # save to disk
          cciaObj$savePops(popType, purge = TRUE, includeFiltered = TRUE)
        }
      } else if (self$funParams()$callingFun %in% c("cellContactsMeshes")) {
        # add populations to popMap
        for (i in self$funParams()$popsA) {
          # get pop type
          # popType <- self$funParams()$popType
          popTypeA <- popTypesFromPops(self$funParams()$popsA)
          # popTypeB <- popTypesFromPops(self$funParams()$popsB)
          
          parentPops <- popPathsFromPops(c(i))
          pops <- lapply(
            self$funParams()$popsB,
            function(x) {
              list(
                filterMeasure = sprintf(
                  "%s.cell.contact#%s", popTypeA, x),
                filterValues = TRUE,
                filterFun = "eq"
              )
            }
          )
          
          # set names
          names(pops) <- sprintf("contact.%s", str_replace(self$funParams()$popsB, "/", "__"))
          
          # remove populations
          cciaObj$delPopsByPath(
            popTypeA,
            pops = levels(interaction(parentPops, names(pops), sep = "/")),
            includeFiltered = TRUE
          )
          
          # get value name from parent
          popValueName <- cciaObj$popAttr(
            popTypeA,
            "valueName", popPath = i, includeFiltered = TRUE)[[1]]
          
          # add populations
          cciaObj$addFilteredPops(popTypeA, parentPops, pops,
                                  valueName = popValueName)
          
          # save to disk
          cciaObj$savePops(popTypeA, purge = TRUE, includeFiltered = TRUE)
        }
      } else if (self$funParams()$callingFun %in% c("popsContainedInShapes")) {
        # add populations to popMap
        for (i in self$funParams()$pops) {
          popType <- self$funParams()$popType
          parentPops <- c(i)
          
          pops <- list()
          for (j in self$funParams()$shapes) { 
            pops[[sprintf("is.in.%s", j)]] <- list(
              filterMeasure = sprintf(
                "%s.region.contained.in.%s", self$funParams()$popType, j),
              filterValues = TRUE,
              filterFun = "eq"
            )
            
            pops[[sprintf("not.in.%s", j)]] <- list(
              filterMeasure = sprintf(
                "%s.region.contained.in.%s", self$funParams()$popType, j),
              filterValues = FALSE,
              filterFun = "eq",
              filterDefaultAll = TRUE
            )
          }
            
          # remove populations
          cciaObj$delPopsByPath(
            popType,
            pops = levels(interaction(parentPops, names(pops), sep = "/")),
            includeFiltered = TRUE
          )
          
          # get value name from parent
          popValueName <- cciaObj$popAttr(
            self$funParams()$popType,
            "valueName", popPath = i, includeFiltered = TRUE)[[1]]
          
          # add populations
          cciaObj$addFilteredPops(popType, parentPops, pops,
                                  valueName = popValueName)
          
          # save to disk
          cciaObj$savePops(popType, purge = TRUE, includeFiltered = TRUE)
        }
      }
          
      # save object
      cciaObj$saveState()
    }
  )
)