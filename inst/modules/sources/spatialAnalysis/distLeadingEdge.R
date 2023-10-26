DistLeadingEdge <- R6::R6Class(
  "DistLeadingEdge",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "distLeadingEdge",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start leading edge detection")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get populations
      # popUtils <- cciaObj$popUtils(self$funParams()$popType)
      
      # set column for analysis
      distCol <- paste(
        self$funParams()$popType, "cell", "dist", "leading", "edge", sep = ".")
      
      # get root DT
      # rootDT <- popUtils$popDT()
      rootDT <- cciaObj$popDT(self$funParams()$popType, includeFiltered = TRUE)
      
      # init analysis column with NA
      rootDT[, c(distCol) := as.numeric(NA)]
      
      # remember value names for pops
      # popValueNames <- list()
      
      # go through pops
      # prepare populations
      popsForEdge <- self$funParams()$popsForEdge
      
      for (i in popsForEdge) {
        # This does not return filtered populations
        # popDT <- popUtils$popDT(i)
        popDT <- cciaObj$popDT(self$funParams()$popType, pops = i,
                               includeFiltered = TRUE, completeDT = TRUE)
        # TODO get PPP - is that necessary?
        popPPP <- cciaObj$ppp(popType = self$funParams()$popType, pops = i,
                              includeFiltered = TRUE, completeDT = TRUE)
        
        self$writeLog(self$funParams()$popType)
        self$writeLog(i)
        
        centroidCols <- colnames(popDT)[startsWith(colnames(popDT), "centroid_")]
        convertPixelToPhysical(popDT, cciaObj$omeXMLPixelRes())
        
        # get distances
        popsBbox <- boundingbox(popPPP)
        
        # make convex hull to ppp and use bbox as window
        pointsDF <- data.frame(
          x = popPPP$window$bdry[[1]]$x,
          y = popPPP$window$bdry[[1]]$y
        )
        
        pppChull <- spatstat.geom::as.ppp(pointsDF, W = popsBbox)
        
        # get distances to window
        pointsDF$dist <- bdist.points(pppChull)
        
        headPoint <- pointsDF %>% arrange(-dist) %>% head(1)
        
        # get orientation
        isPortrait <- FALSE
        if (popsBbox$xrange[[2]] < popsBbox$yrange[[2]])
          isPortrait <- TRUE
        
        # split chull
        # TODO cleaner
        if (isPortrait) {
          splitPoint <- diff(popsBbox$yrange)/2
          
          diffBorder <- c(
            abs(diff(c(headPoint$y, popsBbox$yrange[[1]]))),
            abs(diff(c(headPoint$y, popsBbox$yrange[[2]])))
          )
          
          if (diffBorder[[1]] < diffBorder[[2]])
            splitDF <- pointsDF[pointsDF$y < splitPoint,]
          else
            splitDF <- pointsDF[pointsDF$y > splitPoint,]
          
          # buffer points
          splitDF <- splitDF %>%
            group_by(x) %>%
            mutate(
              border.dist = min(
                c(abs(x - popsBbox$xrange[[1]]), abs(x - popsBbox$xrange[[2]]))
              ))
        } else {
          splitPoint <- diff(popsBbox$xrange)/2
          
          diffBorder <- c(
            abs(diff(c(headPoint$x, popsBbox$xrange[[1]]))),
            abs(diff(c(headPoint$x, popsBbox$xrange[[2]])))
          )
          
          if (diffBorder[[1]] < diffBorder[[2]])
            splitDF <- pointsDF[pointsDF$x < splitPoint,]
          else
            splitDF <- pointsDF[pointsDF$x > splitPoint,]
          
          # buffer points
          splitDF <- splitDF %>%
            group_by(y) %>%
            mutate(
              border.dist = min(
                c(abs(y - popsBbox$yrange[[1]]), abs(y - popsBbox$yrange[[2]]))
              ))
        }
                
        # get distance to line
        # https://gis.stackexchange.com/a/360678
        # https://gis.stackexchange.com/a/270894
        # create line from points
        # TODO this is 2D only
        m <- as.matrix(splitDF[splitDF$border.dist > self$funParams()$borderCutoff, c("x", "y")])
        multipoints <- sf::st_multipoint(m, dim = "XY")
        lines <- sf::st_cast(multipoints, "MULTILINESTRING")
        points <- sf::st_as_sf(popDT, coords = c("centroid_x", "centroid_y"), agr = "constant")
        
        # define merge cols
        mergeCols <- c("value_name", "label")
        mergeCols <- mergeCols[mergeCols %in% names(popDT)]
        
        # copy information to root data table
        popDT[, c(distCol) := sf::st_distance(points, lines)]
        
        # join to root
        # https://stackoverflow.com/a/34600831/13766165
        rootDT[popDT[, c(mergeCols, distCol), with = FALSE],
               on = mergeCols,
               c(distCol) := list(
                 get(paste("i", distCol, sep = "."))
               )]
      }
      
      valueNames <- unique(rootDT$value_name)
      obsCols <- c(distCol)
      
      if (is.null(valueNames)) {
        # TODO this is specific to flow when no other gating is added
        labels <- cciaObj$labelProps()
        
        # order DT
        # TODO this should not be necessary?
        # This might be due to multi-layered segmentation merged?
        rootDT[, label := factor(label, levels = labels$values_obs()$label)]
        setkey(rootDT, label)
        
        if (!is.null(labels)) {
          labels$add_obs(reticulate::r_to_py(as.list(rootDT[, ..obsCols])))
          
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
            labels$add_obs(reticulate::r_to_py(as.list(rootDT[value_name == x, ..obsCols])))
            
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
