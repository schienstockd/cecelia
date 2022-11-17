ClusterTracks <- R6::R6Class(
  "ClusterTracks",
  inherit = BehaviourAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "clusterTracks",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog(">> Calculate clustering for cells")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get uIDs
      uIDs <- NULL
      if ("uIDs" %in% names(self$funParams())) {
        uIDs <- self$funParams()$uIDs
      }
      
      # # prep for value retrieval
      # valueNames <- self$funParams()$valueNames
      # names(valueNames) <- valueNames
      
      if (self$funParams()$calcLabelProps == TRUE) {
        self$writeLog(">> Get tracks from images")
        
        # go through populations
        tracks = list()
        for (x in self$funParams()$popsToCluster) {
          self$writeLog(x)
          
          tracks[[x]] <- cciaObj$tracks(
            pop = x, uIDs = uIDs,
            minTracklength = self$funParams()$minTracklength)
        }
        
        # combine data.tables into one
        tracks.DT <- NULL
        if (length(self$funParams()$trackMeasures) > 0) {
          self$writeLog(">> Get population DTs from images")
          
          tracks.DTs <- list()
          
          # get all measurements from tracks
          # TODO should that be a parameter .. ?
          for (measure.x in self$funParams()$trackMeasures) {
            tracks.DTs[[measure.x]] <- tracks.combine.dt(lapply(
              tracks, function(x) tracks.measure.fun(
                x, eval(parse(text = paste0("celltrackR::", measure.x))),
                result.name = measure.x)
              # steps.subtracks = 10)
            ), idcol = "pop")
          }
          
          self$writeLog(">> Combine measurements into one DT")
          
          tracks.DT <- tracks.DTs[[self$funParams()$trackMeasures[[1]]]]
          
          if (length(self$funParams()$trackMeasures) > 1) {
            for (x in tracks.DTs[names(tracks.DTs) != self$funParams()$trackMeasures[[1]]]) {
              tracks.DT <- merge(
                tracks.DT, x,
                by = c("pop", "uID", "track_id"),
                all.x = TRUE
              )
            }
          }
        }
        
        # get individual object parameters
        tracksInfo.DTs <- list()
        
        self$writeLog(">> Get object parameters")
        objectMeasures <- c(
          self$funParams()$objectMeasures,
          self$funParams()$nMeasures,
          self$funParams()$sumMeasures
        )
        
        # add any columns needed to calculate measures
        if (length(self$funParams()$calcMeasures) > 0) {
          # get measures for population type
          # TODO this is very simplistic
          calcMeasures <- sapply(
            self$funParams()$calcMeasures,
            function(x) stringr::str_extract_all(x, "live\\.*([:alnum:]|\\.)+"))
          
          # get base names for measures
          calcCols <- lapply(
            calcMeasures,
            function(x) unique(sapply(x, cciaStatsName, USE.NAMES = FALSE)))
          
          objectMeasures <- c(objectMeasures, unlist(calcCols))
        }
        
        # get quantiles for track measurements
        trackQuantiles <- c(0.95, 0.05)
        
        if ("trackQuantiles" %in% names(self$funParams())) {
          trackQuantiles <- self$funParams()$trackQuantiles
        }
        
        # get track measures
        tracksInfoMeasures <- objectMeasures
        
        if ("tracksInfoMeasures" %in% names(self$funParams())) {
          tracksInfoMeasures <- c(
            tracksInfoMeasures,
            self$funParams()$tracksInfoMeasures
          )
        }
        
        for (x in self$funParams()$popsToCluster) {
          self$writeLog(x)
          
          # get info
          tracksInfo.DTs[[x]] <- cciaObj$tracksInfo(
            tracksInfoMeasures,
            parentPop = .flowPopParent(x),
            uIDs = uIDs,
            addPops = if ("addPops" %in% names(self$funParams())) self$funParams()$addPops else NULL,
            colsToNormalise = cciaConf()$parameters$shapeDescriptors,
            normPercentile = self$funParams()$percentile/100,
            quantiles = trackQuantiles
          )
        }
        
        # bind together
        tracksInfo.DT <- data.table::rbindlist(tracksInfo.DTs, fill = TRUE, idcol = "pop")
        
        # bind to tracks
        if (!is.null(tracks.DT)) {
          tracks.DT[, track_id := as.double(track_id)]
          
          tracks.DT <- merge(
            tracks.DT, tracksInfo.DT,
            by = c("pop", "uID", "track_id"),
            all.x = TRUE
          )
        } else {
          tracks.DT <- tracksInfo.DT
        }
        
        # calculate measures
        if (length(self$funParams()$calcMeasures) > 0) {
          # go through measurements and calculate
          for (i in names(self$funParams()$calcMeasures)) {
            x <- self$funParams()$calcMeasures[[i]]
            
            tracks.DT[, c(i) := eval(parse(text = x))]
          }
        }
        
        # replace NA with 0
        # TODO is that ok?
        # or is there any measure where NA makes sense?
        tracks.DT[is.na(tracks.DT)] <- 0
        
        # get non-zero columns
        # https://stackoverflow.com/a/21530306/13766165
        nonZeroCols <- tracks.DT[, colSums(tracks.DT != 0, na.rm = TRUE) > 0]
        nonZeroCols <- nonZeroCols[nonZeroCols == TRUE]
        
        # save as anndata
        cciaEnv()$LabelPropsUtils(
          self$envParams()$dirs$task,
          value_name = self$funParams()$valueName)$label_props(
            # tracks.DT %>% drop_na(),
            # tracks.DT,
            tracks.DT[, ..nonZeroCols],
            save = TRUE,
            obs_cols = list("uID", "track_id", "pop")
            )
      }
      
      # which measures are logical?
      measureTypes <- sapply(self$funParams()$objectMeasures, .cciaStatsTypeIsCategorical)
      
      # get object
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        trackMeasures = self$funParams()$trackMeasures,
        objectMeasures = self$funParams()$objectMeasures[measureTypes == FALSE],
        logicalMeasures = self$funParams()$objectMeasures[measureTypes == TRUE],
        nMeasures = self$funParams()$nMeasures,
        sumMeasures = self$funParams()$sumMeasures,
        calcMeasures = as.list(names(self$funParams()$calcMeasures)),
        resolution = self$funParams()$resolution,
        percentile = self$funParams()$percentile,
        pagaThreshold = self$funParams()$pagaThreshold,
        usePaga = self$funParams()$usePaga
      )
      
      # call python
      self$pyScript("cluster_tracks", params)
      
      # load results
      clusterResults <- as.data.table(
        cciaEnv()$LabelPropsUtils(
          self$envParams()$dirs$task,
          value_name = sprintf("%s.sc", self$funParams()$valueName)
          )$label_props_view()$as_df()
        )
      
      self$writeLog(">> Save clusters back to images")
      
      # add back to labels
      # go through objects
      for (x in cciaObj$cciaObjects(uIDs = uIDs)) {
        self$writeLog(sprintf("> Save %s", x$getUID()))
        
        # go through pops
        for (y in unique(clusterResults[uID == x$getUID(),]$pop)) {
          self$writeLog(y)
          
          # get value name
          popValueName <- x$popAttr(
            self$funParams()$popType,
            "valueName", popPath = y, includeFiltered = TRUE
          )[[1]]
          
          # save back to labels
          labels <- x$labelProps(valueName = popValueName)
          
          # get cell state information
          mergedDT <- clusterResults[pop == y & uID == x$getUID(), c(
            "track_id", "clusters"
          )][
            as.data.table(labels$values_obs()), 
            on = .(track_id)
          ]
          
          # set cluster column
          clusterCol <- sprintf(
            "%s.cell.track.clusters.%s",
            self$funParams()$popType, self$funParams()$clusterColName
          )
          
          setnames(mergedDT, "clusters", clusterCol)
          
          # push back to labels
          labels$add_obs(as.list(mergedDT[, ..clusterCol]))
          
          labels$save()
          labels$close()
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
