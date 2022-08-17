source(file.path(
  cfg$tasks$sources, "spatialAnalysis.R")
)

CellRegions <- R6::R6Class(
  "CellRegions",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellRegions",
        sep = CCID_CLASS_SEP
      )
    },
    
    # detect regions
    detectRegions = function(x) {
      # get regions
      regions <- frNN(x, eps = self$funParams()$regionRadius)
      
      resultList <- list()
      
      # go through all cells
      for (i in names(regions$id)) {
        # add the cell itself to the region
        if (self$funParams()$addSelf == TRUE) {
          regions$id[[i]] <- c(regions$id[[i]], which(rownames(x) == i))
          regions$dist[[i]] <- c(regions$dist[[i]], 0)
        }
      }
      
      # convert to datatable
      data.table::rbindlist(lapply(
        names(regions$id),
        function(i) {
          numCellsInRegion <- length(regions$id[[i]])
          
          if (numCellsInRegion > 1) {
            # get value name and label for main
            mainIdSplit <- str_split(i, "\\#")[[1]]
            
            # get value name and label for neighbour
            neighbourIdSplit <- str_split(rownames(x)[regions$id[[i]]], "\\#")
            
            # create table
            # https://stackoverflow.com/a/28630369/13766165
            return(data.table::rbindlist(Map(
              data.table,
              value_name = rep(mainIdSplit[[1]], numCellsInRegion),
              label = rep(as.numeric(mainIdSplit[[2]]), numCellsInRegion),
              neighbour_value_name = unlist(purrr::map(neighbourIdSplit, 1)),
              neighbour_label = unlist(purrr::map(neighbourIdSplit, 2)),
              neighbour_dist = regions$dist[[i]]
            )))
         }
        }))
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start region detection")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get populations
      # popUtils <- cciaObj$popUtils(self$funParams()$popType)

      # get root DT
      # this assumes that all cells within that
      # dataframe will be used for region seeds
      # rootDT <- popUtils$popDT()
      popDT <- cciaObj$popDT(
        self$funParams()$popType, self$funParams()$pops,
        includeFiltered = TRUE
      )

      # get centroids
      centroidCols <- colnames(popDT)[startsWith(colnames(popDT), "centroid_")]

      # exclude T
      centroidCols <- centroidCols[centroidCols != "centroid_t"]

      popDT[, ..centroidCols]

      # convert pixels to physical
      convertPixelToPhysical(popDT, cciaObj$omeXMLPixelRes())

      # build result list
      regionNN <- NULL

      # go through timepoints if should be used
      if (self$funParams()$perTimepoint == TRUE && "centroid_t" %in% centroidCols) {
        regionNN <- list()

        for (t in seq(0, max(popDT$centroid_t))) {
          # convert centroid columns to matrix
          x <- as.matrix(popDT[centroid_t == t, ..centroidCols])

          # set names
          rownames(x) <- paste(
            popDT[centroid_t == t, ]$value_name,
            popDT[centroid_t == t, ]$label,
            sep = "#"
            )
          
          # detect clusters
          regionNN[[t + 1]] <- self$detectRegions(x)
        }

        # bind
        regionNN <- data.table::rbindlist(regionNN, idcol = "centroid_t")
        regionNN <- regionNN[, centroid_t := centroid_t - 1]
      } else {
        # convert centroid columns to matrix
        x <- as.matrix(popDT[, ..centroidCols])
        
        # TODO this should be retrieved from pop utils?
        # rather than setting 'default'
        valueNames <- popDT$value_name
        if (is.null(valueNames)) {
          valueNames <- "default"
        }
        
        # set names
        rownames(x) <- paste(
          valueNames,
          popDT$label,
          sep = "#"
        )
        
        # detect clusters
        regionNN <- self$detectRegions(x)
      }

      # convert columns to numeric
      # https://stackoverflow.com/a/29495536/13766165
      numericCols <- c("neighbour_label", "neighbour_dist")
      regionNN[, (numericCols) := lapply(.SD, as.numeric), .SDcols = numericCols]
      
      # save to file
      cciaEnv()$LabelPropsUtils(
        self$envParams()$dirs$task,
        file.path(
          cfg$dirs$tasks$labelProps, self$regionsFilename()
        )
        )$label_props(
          r_to_py(regionNN),
          save = r_to_py(TRUE),
          obs_cols = r_to_py(list(
            "value_name", "label",
            "neighbour_value_name",
            "neighbour_label",
            "neighbour_dist"
          ))
          )

      cciaObj$saveState()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
