CreateGatingSet <- R6::R6Class(
  "CreateGatingSet",
  inherit = GatePopulations,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "createGatingSet",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # filter channel names
    filterChannelNames = function(channelNames, filterNames) {
      # check with selected channel names
      acceptedChannels <- channelNames %in% filterNames
      
      if (!is.null(attr(channelNames, "types"))) {
        for (i in attr(channelNames, "types"))
          acceptedChannels <- acceptedChannels | channelNames %in% paste0(i, "_", filterNames) 
      }
      
      channelNames <- channelNames[acceptedChannels]
    },
    
    # run
    run = function() {
      # get value name
      valueName <- self$funParams()$valueName
      
      # reset image information
      self$resetImageInfo(gatingSet = valueName)
      
      self$initLog()
      self$writeLog(sprintf(
        "Start creating gatingSet with '%s' transformation",
        self$funParams()$transformation
        ))
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create gating set
      if (cciaObj$getCciaClass() == "CciaImageSet") {
        ffs <- list()
        channelNames <- c()
        
        # get fcs files
        for (x in cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)) {
          ffs <- append(
            ffs,
            x$flowFrame(
              valueName = valueName,
              compensateZ = self$funParams()$compensateZ,
              cropDataBySD = self$funParams()$cropDataBySD,
              polyDegree = self$funParams()$polyDegree,
              applyReversedLog = self$funParams()$applyReversedLog,
              reversedLogBase = self$funParams()$reversedLogBase
            ))
          
          # channel names common across images
          if (length(channelNames) > 0)
            channelNames <- channelNames[channelNames %in% x$imChannelNames(includeTypes = TRUE)]
          else
            channelNames <- x$imChannelNames(includeTypes = TRUE)
        }
        
        names(ffs) <- self$funParams()$uIDs
        
        if (length(self$funParams()$transChannels) > 0) {
          # check with selected channel names
          channelNames <- self$filterChannelNames(
            channelNames, self$funParams()$transChannels)
        }
        
        self$writeLog(paste(
          ">> Tranform", paste(channelNames, collapse = ", ")))
        
        gs <- .flowCreateGatingSet(
          ffs,
          channelNames,
          self$funParams()$transformation,
          transChannels = self$funParams()$transChannels
        )
      } else {
        # get channel names
        channelNames <- cciaObj$imChannelNames(includeTypes = TRUE)
        
        self$writeLog(paste(
          ">> BEFORE", paste(channelNames, collapse = ", ")))
        self$writeLog(paste(attr(channelNames, "types"), collapse = ", "))
        
        if (length(self$funParams()$transChannels) > 0) {
          channelNames <- self$filterChannelNames(
            channelNames, self$funParams()$transChannels)
        }
        
        self$writeLog(paste(
          ">> Transform", paste(channelNames, collapse = ", ")))
        
        # create gs
        gs <- .flowCreateGatingSet(
          cciaObj$flowFrame(
            valueName = valueName,
            compensateZ = self$funParams()$compensateZ,
            cropDataBySD = self$funParams()$cropDataBySD,
            polyDegree = self$funParams()$polyDegree,
            applyReversedLog = self$funParams()$applyReversedLog,
            reversedLogBase = self$funParams()$reversedLogBase
          ),
          channelNames,
          self$funParams()$transformation,
          ffNames = cciaObj$getUID(),
        )
      }
      
      # save gs
      gsPath <- file.path(
        self$envParams()$dirs$task,
        taskDirFiles("data", paste0(valueName, cciaConf()$files$ext$gatingSet))
      )
      
      # remove gating set before saving
      unlink(gsPath, recursive = TRUE)
      
      flowWorkspace::save_gs(gs, gsPath, overwrite = TRUE)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo(gatingSet = valueName)
    }
  )
)
