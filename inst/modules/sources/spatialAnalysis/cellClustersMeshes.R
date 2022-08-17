source(file.path(
  cfg$tasks$sources, "spatialAnalysis.R")
)

CellClustersMeshes <- R6::R6Class(
  "CellClustersMeshes",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellClustersMeshes",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start cluster detection")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imRes = cciaObj$omeXMLPixelRes(),
        is3D = cciaObj$omeXMLPixels()$SizeZ > 1,
        isTimecourse = cciaObj$omeXMLPixels()$SizeT > 1,
        popType = self$funParams()$popType,
        popsToCluster = self$funParams()$popsToCluster,
        minCellNum = self$funParams()$minCellNum,
        maxClusterDist = self$funParams()$maxClusterDist,
        noiseFilter = self$funParams()$noiseFilter,
        timepoints = if ("timepoints" %in% names(self$funParams()))
          self$funParams()$timepoints
        else
          seq(cciaObj$omeXMLPixels()$SizeT) - 1
      )
      
      # save pops before call
      cciaObj <- self$cciaTaskObject()
      cciaObj$savePopMap(self$funParams()$popType, includeFiltered = TRUE)
      cciaObj$savePops(self$funParams()$popType, purge = TRUE, includeFiltered = TRUE)
      
      # call python
      self$pyScript("cell_clusters_mesh", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
