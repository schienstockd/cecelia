CellRegionsStats <- R6::R6Class(
  "CellRegionsStats",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellRegionsStats",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Calculate stats for regions")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get region props
      regionProps <- as.data.table(cciaObj$regionProps(
        valueName = self$funParams()$regionValueName
      )$values_obs())
      
      # focus on subset of cells and neighbours
      regionProps <- regionProps[
        # value_name == self$funParams()$valueName &
          neighbour_value_name %in% self$funParams()$neighbourValueNames]
      
      # merge with labels to get phenotypes
      labelProps <- as.data.table(cciaObj$labelProps(
        valueName = self$funParams()$valueName)$values_obs())
      
      # get column names
      # https://stackoverflow.com/a/45046136/13766165
      labelPropsCols = setdiff(colnames(labelProps), c("label"))
      
      # merge by reference
      regionProps[labelProps, on = .(neighbour_label = label),
                  (labelPropsCols) := mget(labelPropsCols)]
      
      # get columns for type
      typeCols <- labelPropsCols[
        !is.na(stringr::str_match(labelPropsCols, sprintf(
          "%s\\.[^regions]", self$funParams()$popType)))
      ]
      
      # TODO hard code the parameters to quantify
      # ideally, this would be done in a nicer interface
      # not sure where and how
      labelStats = cciaConf()$parameters$labelStats
      
      # check that all label stats are in the data
      labelStats <- labelStats[names(labelStats) %in% colnames(regionProps)]
      
      # make lists of summaries
      obsCols <- c()
      for (i in names(labelStats)) {
        x <- labelStats[[i]]
        
        if (x %in% c("logical", "categorical")) {
          # drop NA for value
          # not by reference yet?
          # https://stackoverflow.com/a/10791729/13766165
          regionProps <- regionProps[!is.na(get(i)), ]
          
          # group
          DT <- regionProps[, .(n = .N), by = .(value_name, label, get(i))]
          
          # average
          DT[, freq := n/sum(n), by = .(value_name, label)]
          
          # prepare new names
          obsColNames <- unique(DT[, get])
          names(obsColNames) <- obsColNames
          obsColNames <- sapply(obsColNames, function(x) sprintf(
            "%s.%s",
            # replace 'cell' with 'region'
            stringr::str_replace(i,
                        sprintf("%s.cell", self$funParams()$popType),
                        sprintf("%s.region", self$funParams()$popType)
                        ), x
            ))
          
          # cast wider
          # https://stackoverflow.com/a/65048192/13766165
          DT <- dcast(DT, value_name+label~get, value.var = "freq")
          
          # rename columns
          for (j in names(obsColNames)) {
            setnames(DT, j, obsColNames[[j]])
            
            # add to obs cols
            # obsCols[[uniqueVals[[j]]]] <- statsCols[[j]]
          }
        } else if (x == "numeric") {
          # group and average
          DT <- regionProps[, .(freq = mean(get(i))), by = .(value_name, label)]
          
          # replace 'cell' with 'region'
          obsColNames <- stringr::str_replace(
            i,
            sprintf("%s.cell", self$funParams()$popType),
            sprintf("%s.region", self$funParams()$popType)
          )
          
          # rename
          setnames(DT, "freq", obsColNames)
        }
        
        # merge to labels
        labelProps[DT, on = .(label),
                   (obsColNames) := mget(obsColNames)]
        
        # add names to list
        obsCols <- c(obsCols, obsColNames)
      }
      
      # go through distance measures
      if (all(
        length(self$funParams()$distFromPops) > 0,
        length(self$funParams()$distToPops) > 0
        )) {
        # get population DTs for 'from'/'to'
        fromDT <- cciaObj$popDT(
          self$funParams()$popType,
          pops = self$funParams()$distFromPops,
          includeFiltered = TRUE)
        
        toDT <- cciaObj$popDT(
          self$funParams()$popType,
          pops = self$funParams()$distToPops,
          includeFiltered = TRUE)
        
        # merge with region props
        mergeCols <- c("label")
        if ("value_name" %in% colnames(fromDT)) {
          mergeCols <- c("value_name", mergeCols)
        }
        
        regionProps[fromDT[, mget(c(mergeCols, "pop"))], on = mergeCols,
                    pop := pop]
        
        # TODO ..
      }
      
      # create a column and add to label props
      cciaObj$labelProps(valueName = self$funParams()$valueName)$add_obs(
        reticulate::r_to_py(
          as.list(labelProps[, ..obsCols])
        )
      )$save()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
