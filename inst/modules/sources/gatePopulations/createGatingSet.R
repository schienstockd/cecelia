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
        sep = CCID_CLASS_SEP
      )
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
        
        gs <- .flowCreateGatingSet(
          ffs,
          channelNames,
          self$funParams()$transformation
        )
      } else {
        self$writeLog("A")
        gs <- .flowCreateGatingSet(
          cciaObj$flowFrame(
            valueName = valueName,
            compensateZ = self$funParams()$compensateZ,
            polyDegree = self$funParams()$polyDegree,
            applyReversedLog = self$funParams()$applyReversedLog,
            reversedLogBase = self$funParams()$reversedLogBase
          ),
          cciaObj$imChannelNames(includeTypes = TRUE),
          self$funParams()$transformation,
          ffNames = cciaObj$getUID()
        )
        self$writeLog("B")
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
