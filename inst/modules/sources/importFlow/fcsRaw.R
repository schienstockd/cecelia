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
      paramsDF$desc[!naNames] <- paste(paramsDF$name[!naNames], paramsDF$desc[!naNames], sep = "-")
      
      self$writeLog(paste(">> Convert to DT"))
      
      # compensate data
      # TODO this has to be interactive
      if (self$funParams()$applyAutospillComp == TRUE) {
        # Generate compensation matrix with autospill
        # https://autospill.vib.be/public/#/results/7418326b0127a8a587c276a1bcb39608
        comp_matrix <- read.csv(
          self$funParams()$fileAutospillComp, header = TRUE)
        
        # move first column to rownames
        rownames(comp_matrix) <- comp_matrix[, 1]
        comp_matrix[, 1] <- NULL
        
        # adjust column names
        colnames(comp_matrix) <- rownames(comp_matrix)
        
        comp <- flowCore::compensation(comp_matrix)
        cf <- compensate(cf, comp)
      }
      
      # convert to DT
      DT <- fortify(cf)
      
      # remove unwanted columns
      keepCols <- colnames(DT)
      keepCols <- keepCols[!keepCols %in% c(".rownames", "name")]
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
      
      cciaObj$setImLabelPropsFilepath(
        paste0(self$funParams()$valueName, cciaConf()$files$ext$labelProps),
        valueName = self$funParams()$valueName
      )
      
      # set channel names
      cciaObj$setImChannelNames(paramsDF$desc, checkLength = FALSE)
      
      # save
      cciaObj$saveState()
      
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
