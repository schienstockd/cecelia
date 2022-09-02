HmmHybrid <- R6::R6Class(
  "HmmHybrid",
  inherit = BehaviourAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "hmmHybrid",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Calculate HMM combinations")
      
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
        popCols = c("cell_id", hmmCols),
        includeFiltered = TRUE,
        colsToNormalise = cciaConf()$parameters$shapeDescriptors,
        uIDs = uIDs
      )
      
      # get hybrid state
      # https://stackoverflow.com/a/33015233/13766165
      tracks.DT[!is.na(get(hmmCols)),
                tmp.hybrid := Reduce(function(...) paste(..., sep = "."), .SD[, mget(hmmCols)])]
      
      # go through objects
      for (x in cciaObj$cciaObjects(uIDs = uIDs)) {
        self$writeLog(sprintf("save %s", x$getUID()))
        
        # go through value names
        for (j in unique(tracks.DT[uID == x$getUID(),]$value_name)) {
          # save back to labels
          labels <- x$labelProps(valueName = j)
          
          # get cell state information
          mergedDT <- tracks.DT[value_name == j & uID == x$getUID(), c(
            "track_id", "cell_id", "tmp.hybrid"
          )][
            as.data.table(labels$values_obs()), 
            on = .(track_id, cell_id)
          ]
          
          hmmCol <- sprintf("live.cell.hmm.hybrid.%s", self$funParams()$colName)
          setnames(mergedDT, "tmp.hybrid", hmmCol)
          
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
