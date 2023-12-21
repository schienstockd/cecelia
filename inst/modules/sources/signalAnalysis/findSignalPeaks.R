FindSignalPeaks <- R6::R6Class(
  "FindSignalPeaks",
  inherit = SignalAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "findSignalPeaks",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start signal detection")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # set column for analysis
      signalNormCol <- paste(
        self$funParams()$popType, "cell", "peak", "norm", self$funParams()$signalName, sep = ".")
      signalRatioCol <- paste(
        self$funParams()$popType, "cell", "peak", "ratio", self$funParams()$signalName, sep = ".")
      signalAccCol <- paste(
        self$funParams()$popType, "cell", "peak", "acc", self$funParams()$signalName, sep = ".")
      signalBooleanCol <- paste(
        self$funParams()$popType, "cell", "has", "peak", self$funParams()$signalName, sep = ".")
      
      # get root DT
      rootDT <- cciaObj$popDT(self$funParams()$popType,
                              includeFiltered = TRUE)
      
      signalAccContactCol <- c()
      
      # if (length(self$funParams()$sumContactWith) > 0) {
      if (!any(stringi::stri_isempty(self$funParams()$sumContactWith))) {
        contactCol <- sprintf(
          "%s.cell.contact#%s",
          self$funParams()$popType,
          self$funParams()$sumContactWith
        )
        
        if (contactCol %in% colnames(rootDT)) {
          signalAccContactCol <- paste(
            self$funParams()$popType, "cell", "peak", "acc",
            sprintf("%s#%s", self$funParams()$signalName, self$funParams()$sumContactWith), sep = ".")
        }
      }
      
      # define columns for joining
      joinCols <- c(
        # signalNormCol,
        signalRatioCol,
        signalBooleanCol,
        signalAccCol,
        signalAccContactCol
      )
      
      # init analysis column with NA
      for (x in joinCols) {
        rootDT[, c(x) := as.numeric(NA)]
      }
      
      # remember value names for pops
      popValueNames <- c()
      
      # go through pops      
      for (i in self$funParams()$pops) {
        # This does not return filtered populations
        popDT <- cciaObj$popDT(self$funParams()$popType, pops = i,
                               includeFiltered = TRUE)
        
        self$writeLog(self$funParams()$popType)
        self$writeLog(i)
        
        # remember value name
        popValueNames <- c(
          popValueNames, unique(popDT$value_name)
        )
        
        # order by track IDs and time
        setorder(popDT, track_id, centroid_t)
        
        # calculate accumulation of calcium over time for each track
        # this might be better than taking a spike detection
        # https://www.nature.com/articles/nature13803
        # To gauge the overall productivity of contact events involving
        # the two B-cell types, the mean ΔRt/R0 of each contact was
        # multiplied by its duration to obtain the area under the
        # curve as the time-integrated calcium response index (TICRI; Fig. 1k).
        
        # normalise channels via percentile
        channelSignal <- .flowCorrectChannelNames(self$funParams()$channelSignal)
        channelDivision <- .flowCorrectChannelNames(self$funParams()$channelDivision)
        channelSubtract <- .flowCorrectChannelNames(self$funParams()$channelSubtract)
        
        # normalise for each track along time
        # popDT[, c(signalNormCol) :=
        #         get(channelSignal) - min(.SD[[channelSignal]]),
        #       by = track_id]
        # popDT[, c(channelDivision) :=
        #         get(channelDivision) - min(.SD[[channelDivision]]),
        #       by = track_id]
        
        # normalise to mean
        meanSignal <- mean(popDT[[channelSignal]], na.rm = TRUE)
        meanDivison <- mean(popDT[[channelDivision]], na.rm = TRUE)
        
        self$writeLog(sprintf("> Mean signal %0.2f", meanSignal))
        self$writeLog(sprintf("> Mean division %0.2f", meanDivison))
        
        # take ratio
        popDT[, c(signalRatioCol) := ((get(channelSignal)/meanSignal) + 1)/((get(channelDivision)/meanDivison) + 1)]
        
        meanSignalRatioCol <- mean(popDT[[signalRatioCol]], na.rm = TRUE)
        
        self$writeLog(sprintf("> Mean signal ratio %0.2f", meanSignalRatioCol))
        
        # normalise by minimum
        popDT[, c(signalNormCol) := get(signalRatioCol) - min(.SD[[signalRatioCol]]),
              by = track_id]
        
        # popDT[, c(signalNormCol) :=
        #         get(signalRatioCol) / runmed(.SD[[signalRatioCol]], k = self$funParams()$normOrder),
        #       by = track_id]
        
        # take ratio of channels for peak detection
        if (self$funParams()$channelSubtract != "") {
          # normalise subtract channel
          .normDTCol(popDT, channelSubtract, "tmp.channel.norm",
                    self$funParams()$channelSubtractPercentile/100)
          
          # take into account extra channel, eg/ AF
          popDT[, c(signalNormCol) := get(signalNormCol) * (1 - tmp.channel.norm)]
          # popDT[, c(signalRatioCol) := get(signalRatioCol) * (1 - tmp.channel.norm)]
        } 
        
        # get rolling sum
        popDT[, c(signalAccCol) := frollsum(.SD, n = self$funParams()$normOrder, fill = 0),
              # by = track_id, .SDcols = c(signalNormCol)]
              by = track_id, .SDcols = c(signalRatioCol)]
        
        # # binarise based on relative pixel values
        popDT[, c(signalBooleanCol) := get(signalAccCol) >= self$funParams()$peakThreshold]
        
        # filter by minimum peaks
        popDT[, c(signalBooleanCol) :=
                as.logical(
                  get(signalBooleanCol) * as.numeric(sum(.SD[[signalBooleanCol]]) >= self$funParams()$minPeaks)
                  ),
              by = track_id]
        
        # # detect peaks
        # suppressWarnings({
        #   popDT[, c(signalBooleanCol) := splus2R::peaks(
        #     .SD[[signalNormCol]], span = self$funParams()$peakWindow),
        #     by = track_id]
        # })
        
        # sum accumulated signal when in contact with another cell
        if (length(signalAccContactCol) > 0) {
          popDT[, c(signalAccContactCol) := get(signalAccCol) * get(contactCol)]
        }
        
        # join
        # https://stackoverflow.com/a/34600831/13766165
        # TODO why does this not work to assign by reference?
        rootDT[popDT,
               on = c("value_name", "label"),
               c(joinCols) := lapply(
                 joinCols,
                 function(x) get(paste("i", x, sep = "."))
                 )]
      }
      
      # go through value names
      # for (x in unique(rootDT$value_name)) {
      for (x in popValueNames) {
        # save back to labels
        labels <- cciaObj$labelProps(valueName = x)
        
        if (!is.null(labels)) {
          for (y in joinCols) {
            labels$add_obs(as.list(rootDT[value_name == x, mget(y)]))
          }
          
          # save and close
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
