HmmTransitions <- R6::R6Class(
  "HmmTransitions",
  inherit = BehaviourAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "hmmTransitions",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Calculate HMM transitions")
      
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
      
      # get hmm cols
      hmmCols <- paste(self$funParams()$popType, "cell", "hmm", "state", self$funParams()$hmmStates, sep = ".")
      
      # get labels for populations from images
      tracks.DT <- cciaObj$popDT(
        popType = self$funParams()$popType,
        pops = self$funParams()$pops,
        cols = c("cell_id", hmmCols),
        includeFiltered = TRUE,
        colsToNormalise = cciaConf()$parameters$shapeDescriptors,
        uIDs = uIDs
      )
      
      # get hybrid state
      # https://stackoverflow.com/a/33015233/13766165
      # TODO is there a better way to do this?
      tracks.DT[, tmp.use.for.hybrid := TRUE]
      
      for (x in hmmCols) {
        tracks.DT[is.na(get(x)), tmp.use.for.hybrid := FALSE]
      }
      
      # tracks.DT[!is.na(get(hmmCols)),
      tracks.DT[tmp.use.for.hybrid == TRUE,
                tmp.hybrid := Reduce(
                  function(...) paste(..., sep = "."), .SD[, mget(hmmCols)]
                  )]
      
      # get transitions between states per track
      # https://stackoverflow.com/a/26292059/13766165
      tracks.DT[, tmp.lag := data.table::shift(.SD),
             by = c("uID", "track_id"),
             .SDcols = "tmp.hybrid"]
      
      # get transitions
      if (self$funParams()$includeStart == TRUE) {
        if (self$funParams()$includeSelfTransitions == TRUE) {
          tracks.DT[!is.na(tmp.hybrid), tmp.transitions := paste(tmp.lag, tmp.hybrid, sep = "_")]
        } else {
          tracks.DT[!is.na(tmp.hybrid) & tmp.lag != tmp.hybrid,
                    tmp.transitions := paste(tmp.lag, tmp.hybrid, sep = "_")]
        }
      } else {
        if (self$funParams()$includeSelfTransitions == TRUE) {
          tracks.DT[!is.na(tmp.hybrid) & !is.na(tmp.lag),
                    tmp.transitions := paste(tmp.lag, tmp.hybrid, sep = "_")]
        } else {
          tracks.DT[!is.na(tmp.hybrid) & !is.na(tmp.lag) & tmp.lag != tmp.hybrid,
                    tmp.transitions := paste(tmp.lag, tmp.hybrid, sep = "_")]
        }
      }
      
      # go through objects
      for (x in cciaObj$cciaObjects(uIDs = uIDs)) {
        self$writeLog(sprintf("save %s", x$getUID()))
        
        # go through value names
        for (j in unique(tracks.DT[uID == x$getUID(),]$value_name)) {
          # save back to labels
          labels <- x$labelProps(valueName = j)
          
          # get cell state information
          mergedDT <- tracks.DT[value_name == j & uID == x$getUID(), c(
            "track_id", "cell_id", "tmp.transitions"
          )][
            as.data.table(labels$values_obs()), 
            on = .(track_id, cell_id)
          ]
          
          hmmCol <- sprintf("live.cell.hmm.transitions.%s", self$funParams()$colName)
          setnames(mergedDT, "tmp.transitions", hmmCol)
          
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
