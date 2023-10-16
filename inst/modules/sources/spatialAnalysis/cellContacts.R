CellContacts <- R6::R6Class(
  "CellContacts",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellContacts",
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
      
      # split population types off pops
      popsA <- splitPops(self$funParams()$popsA)
      popsB <- splitPops(self$funParams()$popsB)
      
      # go through pops
      for (x in popsA) {
        xType <- x[[1]]
        xPop <- x[[2]]
        
        # set column for analysis
        minDistCol <- paste(
          xType, "cell", "min_distance", sep = ".")
        hasContactCol <- paste(
          xType, "cell", "contact", sep = ".")
        
        # get DT
        popDTA <- cciaObj$popDT(xType, pops = xPop, includeFiltered = TRUE)
        
        # get root DT
        # TODO you need to get root for that value name
        # This is the case for multi-label pops
        # TODO would you use this function for static images .. ?
        rootDT <- cciaObj$popDT(xType, includeFiltered = TRUE)
        
        # init analysis column with NA
        rootDT[, c(minDistCol) := as.numeric(NA)]
        rootDT[, c(hasContactCol) := FALSE]
        
        # get columns for query
        centroidCols <- colnames(popDTA)[colnames(popDTA) %in% paste0("centroid_", c("z", "y", "x"))]
        
        for (y in popsB) {
          yType <- y[[1]]
          yPop <- y[[2]]
          
          # get DT
          popDTB <- cciaObj$popDT(yType, pops = yPop, includeFiltered = TRUE)
          
          browser()
          
          # run nearest neighbours
          # TODO this is by definition 1
          # do you want a while loop?
          nnRes <- dbscan::kNN(
            popDTB[, ..centroidCols], k = 1, query = popDTA[, ..centroidCols])
          
          # check contact
          x[, shgDist := shgNN$dist]
          x[, shgContact := shgDist < max.dist]
        }
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
