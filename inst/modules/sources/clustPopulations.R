# Base class for mapping methods
ClustPopulations <- R6::R6Class(
  "ClustPopulations",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "clustPopulations"
    },
    
    # reset image information before mapping
    resetImageInfo = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # TODO need to update this after the conversion of 
      # setting versioned variables
      # # unlink datafiles
      # unlink(file.path(
      #   self$envParams()$dirs$task,
      #   cciaConf()$dirs$tasks$data,
      #   cciaConf()$files$anndata))
      # 
      # # reset values
      # cciaObj$setImAnndataFilepath(NULL)
      # 
      # # reset populations
      # cciaObj$setImPopMap("clust", NULL)
      # 
      # # reset plots
      # cciaObj$setImPopGatePlot("clust", plotParams = NULL)
      
      # save object
      cciaObj$saveState()
    },
    
    # update image information after mapping
    updateImageInfo = function() {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create path
      clustPath <- paste0(self$funParams()$valueName, ".clust", cciaConf()$files$ext$anndata)
      
      # update anndata path
      if ("cciaObjects" %in% names(cciaObj)) {
        # remember that this was part of a bigger clustering
        attr(clustPath, "partOf") <- self$funParams()$uIDs
        
        for (x in cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)) {
          x$setImAnndataFilepath(
            clustPath,
            valueName = self$funParams()$valueName
          )
          
          x$saveState()
        }
      } else {
        cciaObj$setImAnndataFilepath(
          clustPath,
          valueName = self$funParams()$valueName
        )
        
        # save object
        cciaObj$saveState()
      }
    }
  )
)
