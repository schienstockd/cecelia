CreateFlowFrame <- R6::R6Class(
  "CreateFlowFrame",
  inherit = GatePopulations,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "createFlowFrame",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get value name
      valueName <- self$funParams()$valueName
      
      # reset image information
      self$resetImageInfo(flowFrame = valueName, gatingSet = valueName)
      
      self$initLog()
      self$writeLog("Start creating flowFrame")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get label properties
      labelsView <- cciaObj$labelProps(valueName = valueName)
      
      # get extra columns
      extraChannelColumns <- c()
      
      extraChannelColumns <- unlist(lapply(
        labelsView$channel_types(), function(x) {
          labelsView$channel_columns(prefix = x)
        }))
      
      # get dataframe
      labelProps <- as.data.table(labelsView$view_label_col()$as_df())
      labelsView$close()
      
      # compensate Z if selected
      if (self$funParams()$compensateZ == TRUE) {
        labelProps <- flowCompensatePoly(
          labelProps, cciaObj$imChannelNames(includeTypes = TRUE),
          "centroid_z", replaceValues = TRUE,
          polyDegree = self$funParams()$polyDegree)
      }
      
      # take reversed log
      if (self$funParams()$applyReversedLog == TRUE) {
        logBase <- self$funParams()$reversedLogBase
        
        # set natural log
        if (logBase == 0)
          logBase <- exp(1)
        
        # go through channels and take double reverse log
        for (x in cciaObj$imChannelNames(includeTypes = TRUE, correctChannelNames = TRUE)) {
          if (x %in% colnames(labelProps)) {
            # take log of reverse
            # avoid -inf
            labelProps[, c(x) := log((-get(x) + max(get(x))) + 1, base = logBase)]
            
            # reverse back
            labelProps[, c(x) := -get(x) + max(get(x))]
          }
        }
      }
      
      # create attributes to add
      propsToAdd <- c(
        extraChannelColumns,
        cciaConf()$fcs$propsToAdd
      )
      
      self$writeLog("> Channels")
      self$writeLog(cciaObj$imChannelNames(includeTypes = TRUE))
      self$writeLog("> Extra properties")
      self$writeLog(propsToAdd)
      
      # add image uID to filename
      # otherwise there is a problem when combining
      # them into one GatingSet
      valueName <- paste(valueName, cciaObj$getUID(), sep = ".")
      
      # create FCS files from label properties
      flowExportToFCS(
        labelProps,
        file.path(
          self$envParams()$dirs$task,
          taskDirFiles("data", paste0(valueName, cciaConf()$files$ext$flowFrame))
          ),
        cciaObj$imChannelNames(includeTypes = TRUE),
        attrNames = propsToAdd,
        channelPattern = cciaConf()$files$labelPropsChannels,
        addRownames = TRUE)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(flowFrame = valueName)
    }
  )
)
