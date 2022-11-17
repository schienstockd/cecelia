CellClusters <- R6::R6Class(
  "CellClusters",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellClusters",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # detect clusters
    detectClusters = function(x) {
      if (nrow(x) >= self$funParams()$clustMinPts) {
        # get clustering
        clusters <- dbscan(
          x, eps = self$funParams()$clustDiameter,
          minPts = self$funParams()$clustMinPts,
          borderPoints = self$funParams()$clustBorderPoints)
        
        # remove clusters below threshold
        # freqClusters <- table(clusters$cluster)
        # clustersToKeep <- as.integer(names(
        #   freqClusters[freqClusters >= self$funParams()$clustMinPts]
        # ))
        # clusters$cluster[!clusters$cluster %in% clustersToKeep] <- 0
        
        clusterIDs <- clusters$cluster
      } else {
        clusterIDs <- rep(0, nrow(x))
      }
      
      # make clusters binary
      isCluster <- clusterIDs
      isCluster[isCluster == 0] <- FALSE
      isCluster[isCluster > 0] <- TRUE
      isCluster <- as.logical(isCluster)
      
      # return
      list(
        ids = clusterIDs,
        logicals = isCluster
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
      
      # get populations
      # popUtils <- cciaObj$popUtils(self$funParams()$popType)
      
      # set column for analysis
      clusterIDCol <- paste(
        self$funParams()$popType, "cell", "clust", "id", sep = ".")
      isClusterCol <- paste(
        self$funParams()$popType, "cell", "is", "clust", sep = ".")
      
      # get root DT
      # rootDT <- popUtils$popDT()
      rootDT <- cciaObj$popDT(self$funParams()$popType,
                              includeFiltered = TRUE)
      
      # init analysis column with NA
      rootDT[, c(clusterIDCol) := as.numeric(NA)]
      rootDT[, c(isClusterCol) := FALSE]
      
      # remember value names for pops
      # popValueNames <- list()
      
      # go through pops
      # prepare populations
      popsToCluster <- self$funParams()$popsToCluster
      
      # combine populations?
      if (self$funParams()$combinePops == TRUE) {
        popsToCluster <- list(popsToCluster)
      }
      
      for (i in popsToCluster) {
        # This does not return filtered populations
        # popDT <- popUtils$popDT(i)
        popDT <- cciaObj$popDT(self$funParams()$popType, pops = i,
                               includeFiltered = TRUE, completeDT = TRUE)
        
        self$writeLog(self$funParams()$popType)
        self$writeLog(i)
        
        centroidCols <- colnames(popDT)[startsWith(colnames(popDT), "centroid_")]
        popDT[, ..centroidCols]
        
        convertPixelToPhysical(popDT, cciaObj$omeXMLPixelRes())
        
        # build result columns
        clusterIDs <- c()
        clusterLogicals <- c()
        
        # go through timepoints if present
        if ("centroid_t" %in% centroidCols) {
          for (t in seq(0, max(popDT$centroid_t))) {
            # convert centroid columns to matrix
            x <- as.matrix(popDT[centroid_t == t, ..centroidCols])
            
            # detect clusters
            detectedClusters <- self$detectClusters(x)
            
            # push to list
            clusterIDs <- c(clusterIDs,
                            detectedClusters$ids)
            clusterLogicals <- c(clusterLogicals,
                                 detectedClusters$logicals)
          }
        } else {
          # convert centroid columns to matrix
          x <- as.matrix(popDT[, ..centroidCols])
          
          # detect clusters
          detectedClusters <- self$detectClusters(x)
          
          # push to list
          clusterIDs <- detectedClusters$ids
          clusterLogicals <- detectedClusters$logicals
        }
        
        # define merge cols
        mergeCols <- c("value_name", "label")
        mergeCols <- mergeCols[mergeCols %in% names(popDT)]
        
        # copy information to root data table
        popDT[, c(clusterIDCol) := clusterIDs]
        popDT[, c(isClusterCol) := clusterLogicals]
        popClusters <- popDT[, c(mergeCols, clusterIDCol, isClusterCol),
                             with = FALSE]
        
        # add value name for pop
        # popValueNames[[i]] <- popDT[pop == i, c("value_name")][1]$value_name
        
        # join
        # https://stackoverflow.com/a/34600831/13766165
        rootDT[popClusters,
               on = mergeCols,
               c(clusterIDCol, isClusterCol) := list(
                 get(paste("i", clusterIDCol, sep = ".")),
                 get(paste("i", isClusterCol, sep = "."))
               )]
      }
      
      valueNames <- unique(rootDT$value_name)
      
      if (is.null(valueNames)) {
        # TODO this is specific to flow when no other gating is added
        labels <- cciaObj$labelProps()
        
        if (!is.null(labels)) {
          labels$add_obs(as.list(rootDT[, ..clusterIDCol]))
          labels$add_obs(as.list(rootDT[, ..isClusterCol]))
          
          # save and close
          labels$save()
          labels$close()
        }
      } else {
        # go through value names
        for (x in valueNames) {
          # save back to labels
          labels <- cciaObj$labelProps(valueName = x)
          
          if (!is.null(labels)) {
            labels$add_obs(as.list(rootDT[value_name == x, ..clusterIDCol]))
            labels$add_obs(as.list(rootDT[value_name == x, ..isClusterCol]))
            
            # save and close
            labels$save()
            labels$close()
          }
        }
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
