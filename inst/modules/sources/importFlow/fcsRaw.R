FcsRaw <- R6::R6Class(
  "FcsRaw",
  inherit = ImportFlow,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "fcsRaw",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      self$resetImageInfo()
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$initLog()
      
      self$writeLog(paste(">> Load FCS"))
      
      # read FCS file
      cf <- flowWorkspace::load_cytoframe_from_fcs(cciaObj$oriFilepath())
      paramsDF <- flowCore::parameters(cf)@data
      naNames <- is.na(paramsDF$desc)
      
      # set to name if not description known
      paramsDF$desc[naNames] <- paramsDF$name[naNames]
      paramsDF$desc[!naNames] <- paste(paramsDF$name[!naNames],
                                       paramsDF$desc[!naNames],
                                       sep = "-")
      # imChannelNames <- c(paramsDF$desc, "label")
      imChannelNames <- paramsDF$desc
      
      # change names
      # TODO because this does not happen from FCS files
      for (i in seq(length(names(cf)))) {
        flowWorkspace::cf_rename_channel(
          cf, names(cf)[[i]], .flowCorrectChannelNames(imChannelNames[[i]]))
      }
      
      self$writeLog(paste(">> Compensate"))
      
      # compensate data
      # TODO this has to be interactive
      if (self$funParams()$applyAutospillComp == TRUE) {
        # get compensation matrix from set
        compSet <- self$initCciaObject(self$funParams()$autospillSetID)

        cf <- flowCore::compensate(cf, compSet$flowAutospillMatrix())
      }

      # add label column to FCS
      cols <- as.integer(seq(nrow(cf)))
      cols <- matrix(cols, dimnames = list(NULL, "label"))
      cf <- flowWorkspace::cf_append_cols(cf, cols)

      # get channel names from cytoframe
      channelNames <- names(cf)

      # transform all channels that are not FSC, SSC, -A, -H, W
      # TODO how does this look for Aurora files .. ?
      transChannels <- channelNames[is.na(
        stringr::str_match(channelNames, "^Time|FSC.|SSC.|label"))]
      
      self$writeLog(paste(
        ">> Transform", paste(transChannels, collapse = ", ")))

      # create gs
      gs <- .flowCreateGatingSet(
        cf,
        transChannels,
        self$funParams()$transformation,
        ffNames = cciaObj$getUID(),
        flowNames = FALSE
      )

      valueName <- self$funParams()$valueName

      # save gs
      gsPath <- file.path(
        self$envParams()$dirs$task,
        taskDirFiles("data", paste0(valueName, cciaConf()$files$ext$gatingSet))
      )

      # remove gating set before saving
      unlink(gsPath, recursive = TRUE)
      flowWorkspace::save_gs(gs, gsPath, overwrite = TRUE)

      # reset populations
      cciaObj$setImPopMap("flow", list())

      self$writeLog(paste(">> Convert to DT"))
      
      # convert to DT
      # Do not use the transformed data
      # DT <- .flowFortifyGs(gs)
      DT <- fortify(cf)
      
      # remove unwanted columns
      keepCols <- colnames(DT)
      keepCols <- keepCols[!keepCols %in% c(".rownames", "name", "label")]
      DT <- DT[, ..keepCols]
      
      self$writeLog(paste(">> Save as Anndata"))
      
      # rename columns as mean_intensity_i
      colnames(DT) <- paste0("mean_intensity_", seq(length(colnames(DT))) - 1)
      
      # save props
      labelsName <- paste0(self$funParams()$valueName, cciaConf()$files$ext$labelProps)
      labelsPath <- file.path("labelProps", labelsName)
      
      # add label as running id
      DT[, label := seq(nrow(DT))]
      
      cciaEnv()$LabelPropsUtils(
        cciaObj$persistentObjectDirectory(),
        labelsPath)$label_props(
          DT, save = TRUE
        )
      
      # set property paths
      cciaObj$setImGatingSetFilepath(
        paste0(self$funParams()$valueName, cciaConf()$files$ext$gatingSet),
        valueName = self$funParams()$valueName
      )
      cciaObj$setImLabelPropsFilepath(
        paste0(self$funParams()$valueName, cciaConf()$files$ext$labelProps),
        valueName = self$funParams()$valueName
      )
      
      # set channel names
      cciaObj$setImChannelNames(imChannelNames, checkLength = FALSE)
      
      # save
      cciaObj$saveState()
      
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
