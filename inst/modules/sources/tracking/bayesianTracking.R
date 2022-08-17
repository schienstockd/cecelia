source(file.path(
  cfg$tasks$sources, "tracking.R")
)

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
        sep = CCID_CLASS_SEP
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
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imRes = cciaObj$omeXMLPixelRes(),
        maxSearchRadius = self$funParams()$maxSearchRadius,
        maxLost = self$funParams()$maxLost,
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
        lambdaTime = self$funParams()$lambdaTime,
        lambdaDist = self$funParams()$lambdaDist,
        thetaTime = self$funParams()$thetaTime,
        thetaDist = self$funParams()$thetaDist,
        filters = self$funParams()$filters
      )

      # call python
      self$pyScript("bayesian_tracking", params)
      
      if (self$funParams()$calcTrackingStats == TRUE) {
        self$writeLog("Calculate tracking stats")
        
        self$writeLog(self$funParams()$valueName)
        
        # get tracks
        tracks <- list()
        tracks[[self$funParams()$valueName]] <- cciaObj$tracks(self$funParams()$valueName)
        
        # filter tracks on user defined thresholds
        removeTrackIDs <- c()
        
        self$writeLog("A")
        
        if (self$funParams()$maxMeanTurningAngle > 0) {
          # get mean angle for tracks and filter
          meanAngleMeasure <- tracks.measure.fun(
            tracks, meanTurningAngle, "meanTurningAngle",
            idcol = "cell_type")
          
          # get filtered IDs
          removeTrackIDs <- meanAngleMeasure[meanTurningAngle > self$funParams()$maxMeanTurningAngle]$track_id
        }
        
        self$writeLog("B")
        
        if (self$funParams()$minDisplacement > 0) {
          # get mean angle for tracks and filter
          displacementMeasure <- tracks.measure.fun(
            tracks, displacement, "displacement",
            idcol = "cell_type")
          
          # get filtered IDs
          removeTrackIDs <- c(
            removeTrackIDs,
            displacementMeasure[displacement < self$funParams()$minDisplacement]$track_id
          )
        }
        
        self$writeLog("C")
        
        # filter tracks
        if (length(removeTrackIDs) > 0) {
          tracks[[self$funParams()$valueName]] <- tracks[[self$funParams()$valueName]][
            !names(tracks[[self$funParams()$valueName]]) %in% removeTrackIDs
          ]
        }
        
        self$writeLog("D")
        
        # create data.table with properties
        tracks.DT <- tracks.measure.fun(tracks, speed, "live.cell.speed",
                                        steps.subtracks = 1,
                                        idcol = "cell_type")
        
        # add further measurements
        tracks.DT[
          tracks.measure.fun(
            tracks, overallAngle, "live.cell.angle",
            steps.subtracks = 2, idcol = "cell_type"),
          on = .(cell_type, track_id, cell_id),
          live.cell.angle := .(live.cell.angle)]
        
        # push back to labels
        labels <- cciaObj$labelProps(valueName = self$funParams()$valueName)
        
        self$writeLog("E")
        
        # get cell state information
        mergedDT <- tracks.DT[, c(
          "track_id", "cell_id", "live.cell.speed", "live.cell.angle"
        )][
          as.data.table(labels$values_obs()),
          on = .(track_id, cell_id)
          ]
        
        # set track id to nan where filtered
        mergedDT[track_id %in% removeTrackIDs, track_id := "NaN"]
        
        # add to labels
        labels$add_obs(
          as.list(mergedDT[, .(track_id, live.cell.speed, live.cell.angle)])
        )
        
        self$writeLog("F")
  
        # save
        labels$save()
        labels$close()
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)