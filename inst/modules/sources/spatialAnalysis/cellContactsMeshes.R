CellContactsMeshes <- R6::R6Class(
  "CellContactsMeshes",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellContactsMeshes",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start contact detection")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imRes = cciaObj$omeXMLPixelRes(),
        is3D = cciaObj$omeXMLPixels()$SizeZ > 1,
        isTimecourse = cciaObj$omeXMLPixels()$SizeT > 1,
        # popType = self$funParams()$popType,
        popsA = self$funParams()$popsA,
        popsB = self$funParams()$popsB,
        invertPopsA = self$funParams()$invertPopsA,
        maxContactDist = self$funParams()$maxContactDist,
        timepoints = if ("timepoints" %in% names(self$funParams()))
          self$funParams()$timepoints
        else
          seq(cciaObj$omeXMLPixels()$SizeT) - 1
      )
      
      # save pops before call
      cciaObj <- self$cciaTaskObject()
      
      # split population types off pops
      popTypes <- popTypesFromPops(c(
        self$funParams()$popsA, self$funParams()$popsB
      ))
      
      # save
      for (x in popTypes) {
        cciaObj$savePopMap(x, includeFiltered = TRUE)
        cciaObj$savePops(x, purge = TRUE, includeFiltered = TRUE)
      }
      
      # call python
      self$pyScript("cell_contacts_mesh", params)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
