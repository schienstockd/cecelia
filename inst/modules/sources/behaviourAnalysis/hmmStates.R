HmmStates <- R6::R6Class(
  "HmmStates",
  inherit = BehaviourAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "hmmStates",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Calculate HMM for cells")
      
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
      
      self$writeLog("Get population DTs")
      
      # get labels for populations from images
      tracks.DT <- cciaObj$popDT(
        popType = self$funParams()$popType,
        pops = self$funParams()$pops,
        popCols = c("cell_id", self$funParams()$modelMeasurements),
        includeFiltered = TRUE,
        colsToNormalise = cciaConf()$parameters$shapeDescriptors,
        uIDs = uIDs
      )
      
      self$writeLog("Filter Noise")
      
      # skip timesteps?
      if ("skipTimesteps" %in% names(self$funParams()) &&
          self$funParams()$skipTimesteps > 0) {
        
        # reset speed and angle
        tracks.DT[, live.cell.speed := NA]
        tracks.DT[, live.cell.angle := NA]
        
        # go through pops
        for (y in self$funParams()$pops) {
          # get tracks
          popTracks <- cciaObj$tracks(y, uIDs = uIDs)
          
          # adjust speed
          if ("live.cell.speed" %in% self$funParams()$modelMeasurements) {
            # get new speed
            tracks.DT[
              tracks.measure.fun(
                popTracks, celltrackR::speed, "live.cell.speed",
                steps.subtracks = 1 + self$funParams()$skipTimesteps,
                steps.overlap = if (self$funParams()$subtrackOverlap == TRUE)
                  self$funParams()$skipTimesteps
                else
                  0,
                idcol = "uID"),
              on = .(uID, track_id, cell_id),
              live.cell.speed := .(i.live.cell.speed)]
          }
          
          # adjust angle
          if ("live.cell.angle" %in% self$funParams()$modelMeasurements) {
            # determine steps if overlap is false
            subtrackSteps <- 2 + self$funParams()$skipTimesteps
            subtrackOverlap <- self$funParams()$skipTimesteps + 1
            
            if (self$funParams()$subtrackOverlap == FALSE) {
              subtrackOverlap <- 0
              
              # align steps with speed
              subtrackSteps <- 1 + self$funParams()$skipTimesteps
            }
            
            # go through pops
            for (y in self$funParams()$pops) {
              # get new angle
              tracks.DT[
                tracks.measure.fun(
                  popTracks, celltrackR::overallAngle, "live.cell.angle",
                  steps.subtracks = subtrackSteps,
                  steps.overlap = subtrackOverlap,
                  idcol = "uID"),
                on = .(uID, track_id, cell_id),
                live.cell.angle := .(i.live.cell.angle)]
            }
          }
        }
      }
      
      # drop cells at the beginning of the track
      for (x in c("live.cell.speed", "live.cell.angle")) {
        if (x %in% self$funParams()$modelMeasurements) {
          tracks.DT <- tracks.DT %>%
            drop_na(all_of(x)) %>%
            # drop inf
            # https://stackoverflow.com/a/55198108/13766165
            filter_all(all_vars(!is.infinite(.)))
        }
      }
      
      # filter noise for measurements
      if (self$funParams()$noiseFilterMeasurements > 0) {
        tracks.DT[,
                  (self$funParams()$modelMeasurements) := lapply(
                    self$funParams()$modelMeasurements,
                    function(x) caTools::runmean(
                    # function(x) runmed(
                      .SD[[x]],
                      k = self$funParams()$noiseFilterMeasurements)
                  ),
                  by = .(pop, uID, track_id)
        ]
      }
      
      # build model
      hmm_model <- depmixS4::depmix(
        lapply(
          self$funParams()$modelMeasurements,
          function(x) eval(parse(text = sprintf("%s~1", x)))
          ),
        data = tracks.DT[, mget(self$funParams()$modelMeasurements)],
        nstates = self$funParams()$numStates,
        ntimes = tracks.DT[, .(num.cells = .N),
                           by = .(value_name, uID, track_id)]$num.cells,
        # gaussian by default
        family = rep(list(gaussian()), length(self$funParams()$modelMeasurements))
      )
      
      # fit
      if ("seed" %in% names(self$funParams())) {
        set.seed(self$funParams()$seed)
      }
      
      self$writeLog("Fit model")
      
      hmm_fit <- depmixS4::fit(hmm_model)
      
      self$writeLog("Predict states")
      
      # predict
      hmm_predict <- depmixS4::posterior(hmm_fit)
      
      # add to tracks
      tracks.DT[, hmm.state := hmm_predict$state]
      
      # go through objects
      for (x in cciaObj$cciaObjects(uIDs = uIDs)) {
        self$writeLog(sprintf("save %s", x$getUID()))
        
        # go through value names
        for (j in unique(tracks.DT[uID == x$getUID(),]$value_name)) {
          self$writeLog(sprintf("> %s", j))
          
          # save back to labels
          labels <- x$labelProps(valueName = j)
          
          # get cell state information
          mergedDT <- tracks.DT[value_name == j & uID == x$getUID(), c(
            "track_id", "cell_id", "hmm.state"
          )][
            as.data.table(labels$values_obs()), 
            on = .(track_id, cell_id)
          ]
          
          # inject states
          if (length(self$funParams()$appendStates) > 0) {
            counter <- 1
            
            for (stateCol in names(self$funParams()$appendStates)) {
              stateVal <- self$funParams()$appendStates[[stateCol]]
              
              if (stateCol %in% names(mergedDT))
                mergedDT[get(stateCol) == stateVal,
                         hmm.state := self$funParams()$numStates + counter]
              
              counter <- counter + 1
            }
          }
          
          # smooth states
          if (self$funParams()$postFiltering > 0) {
            self$writeLog("Post filtering")
            
            find.freq <- function(x) {
              # get most frequent value
              y <- DescTools::Mode(x, na.rm = TRUE)
              
              # TODO take mid occurence value if more than one
              # assume centre for window
              if (length(y > 1)) {
                # names(y) <- y
                # minPos <- sapply(y, function(z) min(which(x == z)))
                # y <- as.numeric(names(y)[which(minPos == min(minPos))])
                # y <- x[[round(length(x)/2)]]
                # take the first one
                y <- y[[1]]
              }
              
              y
            }
            
            # TODO is there a better way?
            for (i in seq(self$funParams()$postIterations)) {
              # for every timepoint, take the value that is most frequent around this window
              # tracks.DT[, hmm.state := frollapply(
              mergedDT[, hmm.state := frollapply(
                x = .SD[, hmm.state], n = self$funParams()$postFiltering,
                find.freq, fill = NA, align = "center"),
                # by = .(pop, uID, track_id)]
                by = .(track_id)]
            }
          }
          
          hmmCol <- sprintf("live.cell.hmm.state.%s", self$funParams()$colName)
          setnames(mergedDT, "hmm.state", hmmCol)
          
          # push back to labels
          labels$add_obs(
            as.list(mergedDT[, ..hmmCol])
          )
          
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
