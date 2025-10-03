ApplyClassifier <- R6::R6Class(
  "ApplyClassifier",
  inherit = Objcl,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "applyClassifier",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start pixel classifier")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # convert classifier channels to numbers
      # and get classifier file paths
      clMapping <- mapply(
        function(x, i) {
          # channel names
          if (length(x$clChannels) > 0) {
            x$clChannels <- sapply(
              x$clChannels, function(y) {
                unname(which(cciaObj$imChannelNames() == y)) - 1
              }, USE.NAMES = FALSE
            )
            
            # classifier filepath
            x$clPath <- file.path(
              cciaObj$persistentObjectDirectory(root = TRUE),
              cecelia:::CCID_IMAGE_COLLECTION,
              cciaConf()$dirs$tasks$classifications,
              cciaConf()$dirs$classifications$pix,
              paste0(i, ".cl")
            )
            x
          } else {
            NULL
          }
        },
        self$funParams()$clMapping, names(self$funParams()$clMapping),
        SIMPLIFY = FALSE
        )
      clMapping <- clMapping[lengths(clMapping) > 0]
      
      # TODO this takes a long time as it loads all objects
      # pixFiles <- self$cciaImageCollection()$pixclFiles(fullPath = TRUE)
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = cciaObj$imFilepath(),
        # clChannels = clChannels,
        # clPath = pixFiles[[self$funParams()$clFile]],
        # clPath = clPath,
        clMapping = clMapping,
        normaliseImage = self$funParams()$normaliseImage,
        normPercentile = self$funParams()$normPercentile,
        minObjectSize = self$funParams()$minObjectSize,
        saveMeshes = self$funParams()$saveMeshes,
        extendedMeasures = self$funParams()$extendedMeasures
      )
      
      # call python
      self$pyScript("apply_classifier", params)
      
      popType <- "clsf"
      
      # go through classifiers
      for (i in names(clMapping)) {
        x <- clMapping[[i]]
        valueName <- paste0(i, ".cl")
        
        # add labels to image
        cciaObj$setImLabelsFilepath(
          paste0(valueName, cciaConf()$files$ext$labels),
          valueName = valueName,
          setDefault = FALSE
        )
        
        # add properties
        cciaObj$setImLabelPropsFilepath(
          paste0(valueName, cciaConf()$files$ext$labelProps),
          valueName = valueName,
          setDefault = FALSE
        )
        
        # create classification populations
        popDT <- cciaObj$popDT(popType, pops = c(valueName), popCols = c("clsf"))
        
        # add children
        pops <- list()
        parentPops <- valueName
        for (i in unique(popDT$clsf)) {
          pops[[xfun::numbers_to_words(i)]] <- list(
            filterMeasure = "clsf",
            filterValues = i,
            filterFun = "eq"
          )
        }
        
        # remove populations
        cciaObj$delPopsByPath(
          popType,
          pops = levels(interaction(parentPops, names(pops), sep = "/")),
          includeFiltered = TRUE
        )
        
        # add populations
        cciaObj$addFilteredPops(popType, parentPops, pops,
                                valueName = valueName)
        
        # save to disk
        cciaObj$savePops(popType, purge = TRUE, includeFiltered = TRUE)
      }
      
      cciaObj$saveState()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
