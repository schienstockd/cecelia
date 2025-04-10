# Bayesian Tracking
BayesianTracking <- R6::R6Class(
  "BayesianTracking",
  inherit = Tracking,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "bayesianTracking",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start Bayesian Tracking")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get visibility
      filterVisibilities <- self$funParamVisibilities("filters", onlyVisible = TRUE)
      
      filters <- self$funParams()$filters
      
      if (!is.null(filterVisibilities))
        filters <- filters[names(filterVisibilities)]
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imRes = cciaObj$omeXMLPixelRes(),
        maxSearchRadius = self$funParams()$maxSearchRadius,
        maxLost = self$funParams()$maxLost,
        trackBranching = self$funParams()$trackBranching,
        minTimepoints = self$funParams()$minTimepoints,
        accuracy = self$funParams()$accuracy,
        probToAssign = self$funParams()$probToAssign,
        noiseInital = self$funParams()$noiseInital,
        noiseProcessing = self$funParams()$noiseProcessing,
        noiseMeasurements = self$funParams()$noiseMeasurements,
        distThresh = self$funParams()$distThresh,
        timeThresh = self$funParams()$timeThresh,
        segmentationMissRate = self$funParams()$segmentationMissRate,
        lambdaLink = self$funParams()$lambdaLink,
        lambdaBranch = self$funParams()$lambdaBranch,
        lambdaTime = self$funParams()$lambdaTime,
        lambdaDist = self$funParams()$lambdaDist,
        thetaTime = self$funParams()$thetaTime,
        thetaDist = self$funParams()$thetaDist,
        filters = filters
      )

      # call python
      if (self$funParams()$calcTrackingStatsOnly == FALSE)
        self$pyScript("bayesian_tracking", params)
      
      if (self$funParams()$calcTrackingStats == TRUE) {
        self$writeLog("Calculate tracking stats")
        self$writeLog(self$funParams()$valueName)
        
        # get tracks
        tracks <- list()
        tracks[[self$funParams()$valueName]] <- cciaObj$tracks(self$funParams()$valueName)
        
        # filter tracks on user defined thresholds
        removeTrackIDs <- c()
        
        if (self$funParams()$maxMeanTurningAngle > 0) {
          # get mean angle for tracks and filter
          meanAngleMeasure <- tracks.measure.fun(
            tracks, celltrackR::meanTurningAngle, "meanTurningAngle",
            idcol = "cell_type")
          
          # get filtered IDs
          removeTrackIDs <- meanAngleMeasure[meanTurningAngle > self$funParams()$maxMeanTurningAngle]$track_id
        }
        
        if (self$funParams()$minDisplacement > 0) {
          # get mean angle for tracks and filter
          displacementMeasure <- tracks.measure.fun(
            tracks, celltrackR::displacement, "displacement",
            idcol = "cell_type")
          
          # get filtered IDs
          removeTrackIDs <- c(
            removeTrackIDs,
            displacementMeasure[displacement < self$funParams()$minDisplacement]$track_id
          )
        }
        
        # filter tracks
        if (length(removeTrackIDs) > 0) {
          tracks[[self$funParams()$valueName]] <- tracks[[self$funParams()$valueName]][
            !names(tracks[[self$funParams()$valueName]]) %in% removeTrackIDs
          ]
        }
        
        # create data.table with properties
        tracks.DT <- tracks.measure.fun(
          tracks, celltrackR::speed,
          "live.cell.speed", steps.subtracks = 1,
          idcol = "cell_type", increment.cell.id = TRUE)
        
        # add further measurements
        tracks.DT[
          tracks.measure.fun(
            tracks, celltrackR::overallAngle, "live.cell.angle",
            steps.subtracks = 2, idcol = "cell_type",
            increment.cell.id = TRUE),
          on = .(cell_type, track_id, cell_id),
          live.cell.angle := .(live.cell.angle)]
        
        # push back to labels
        labels <- cciaObj$labelProps(valueName = self$funParams()$valueName)
        
        self$writeLog(self$funParams()$valueName)
        
        # get label obs
        label.obs <- as.data.table(
          labels$view_cols(list("label", "track_id", "cell_id"))$as_df(close = FALSE, include_x = FALSE))
        
        # check that there are no Nan cell id
        # these can occure when tracks were edited
        # if (any(is.na(label.obs$cell_id))) {
        label.obs[, cell_id := order(centroid_t), by = track_id]
        
        # get cell state information
        mergedDT <- tracks.DT[, c(
          "track_id", "cell_id", "live.cell.speed", "live.cell.angle"
        )][
          # https://stackoverflow.com/a/58508599
          # recalculate speed and angle
          # as.data.table(labels$values_obs())[, .SD, .SDcols = !c(
          #   "live.cell.speed", "live.cell.angle"
          # )], on = .(track_id, cell_id)
          # as.data.table(labels$values_obs()), on = .(track_id, cell_id)
          label.obs, on = .(track_id, cell_id)
          ]
        
        # set track id to nan where filtered
        mergedDT[track_id %in% removeTrackIDs, track_id := "NaN"]
        
        # add to labels
        labels$add_obs(
          as.list(mergedDT[, .(track_id, cell_id, live.cell.speed, live.cell.angle)])
        )
        
        # save
        labels$save()
        labels$close()
      }
      
      # update image information
      self$updateImageInfo()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
