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
      
      # TODO multiple pops do not work
      # go through pops
      for (x in popsA) {
        xType <- x[[1]]
        xPop <- x[[2]]
        
        # get root DT
        # TODO you need to get root for that value name
        # This is the case for multi-label pops
        # TODO would you use this function for static images .. ?
        rootDT <- cciaObj$popDT(xType, includeFiltered = TRUE)
        
        # get DT
        popDTA <- cciaObj$popDT(xType, pops = xPop, includeFiltered = TRUE)
        
        centroidCols <- colnames(popDTA)[startsWith(colnames(popDTA), "centroid_")]
        convertPixelToPhysical(popDTA, cciaObj$omeXMLPixelRes())
        
        # get columns for query
        centroidCols <- colnames(popDTA)[colnames(popDTA) %in% paste0("centroid_", c("z", "y", "x"))]
        obsCols <- c()
        
        for (y in popsB) {
          yType <- y[[1]]
          yPop <- y[[2]]
          
          yPopCol <- stringr::str_replace_all(yPop, "/", "__")
          
          # set column for analysis
          minDistCol <- paste(
            xType, "cell", paste0("min_distance#", yType), yPopCol, sep = ".")
          hasContactCol <- paste(
            xType, "cell", paste0("contact#", yType), yPopCol, sep = ".")
          contactIdCol <- paste(
            xType, "cell", paste0("contact_id#", yType), yPopCol, sep = ".")
          
          # init analysis column with NA
          rootDT[, c(minDistCol) := as.numeric(NA)]
          rootDT[, c(hasContactCol) := FALSE]
          rootDT[, c(contactIdCol) := as.numeric(NA)]
          
          # get DT
          popDTB <- cciaObj$popDT(yType, pops = yPop, includeFiltered = TRUE)
          
          centroidCols <- colnames(popDTB)[startsWith(colnames(popDTB), "centroid_")]
          convertPixelToPhysical(popDTB, cciaObj$omeXMLPixelRes())
          
          # run nearest neighbours
          # TODO this is by definition 1
          # do you want a while loop?
          nnRes <- dbscan::kNN(
            popDTB[, ..centroidCols], k = 1, query = popDTA[, ..centroidCols])
          
          # define merge cols
          mergeCols <- c("value_name", "label")
          mergeCols <- mergeCols[mergeCols %in% names(popDTA)]
          
          # check contact
          popDTA[, c(contactIdCol) := nnRes$id]
          popDTA[, c(minDistCol) := nnRes$dist]
          popDTA[, c(hasContactCol) := nnRes$dist < self$funParams()$maxContactDist]
          
          popUpdate <- popDTA[, c(
            mergeCols, minDistCol, hasContactCol, contactIdCol), with = FALSE]
          
          # join to root
          # https://stackoverflow.com/a/34600831/13766165
          rootDT[popUpdate,
                 on = mergeCols,
                 c(minDistCol, hasContactCol, contactIdCol) := list(
                   get(paste("i", minDistCol, sep = ".")),
                   get(paste("i", hasContactCol, sep = ".")),
                   get(paste("i", contactIdCol, sep = "."))
                 )]
          
          obsCols <- c(obsCols, minDistCol, hasContactCol, contactIdCol)
        }
        
        valueNames <- unique(rootDT$value_name)
        
        if (is.null(valueNames)) {
          # TODO this is specific to flow when no other gating is added
          # labels <- cciaObj$labelProps()
          labels <- cciaObj$labelProps(valueName = "default")
          
          if (!is.null(labels)) {
            # extend root by labels if size does not match
            # labelsDT <- as.data.table(labels$view_label_col()$values_obs())
            labelsDT <- as.data.table(labels$values_obs())
            
            # TODO is there an in memory version for that?
            # rootDT <- labelsDT[rootDT[, c("label", obsCols)], on = c("label")]
            rootDT <- rootDT[labelsDT, on = c("label")]
            
            # order DT
            # TODO this should not be necessary?
            # This might be due to multi-layered segmentation merged?
            rootDT[, label := factor(label, levels = labels$values_obs()$label)]
            setkey(rootDT, label)
            
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
      }
      
      # update image information
      self$updateImageInfo()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      TRUE
    }
  )
)
