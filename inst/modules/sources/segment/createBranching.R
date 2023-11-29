CreateBranching <- R6::R6Class(
  "CreateBranching",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "createBranching",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      valueName <- self$funParams()$valueName
      branchingName <- paste0(valueName, ".", "branch")
      
      self$resetImageInfo(valueName = branchingName)
      
      self$initLog()
      self$writeLog("Start Branching extraction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get channels from segmentation
      labelChannels <- cciaObj$valueAttr("imLabelsFilepath", "channels", valueName = valueName)
      integrateTimeMode <- cciaObj$valueAttr("imLabelsFilepath", "integrateTimeMode", valueName = valueName)
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = cciaObj$imFilepath(),
        branchingName = branchingName,
        labelChannels = labelChannels,
        integrateTimeMode = integrateTimeMode,
        flattenBranching = self$funParams()$flattenBranching,
        useBorders = self$funParams()$useBorders,
        calcExtended = self$funParams()$calcExtended,
        calcFlattened = self$funParams()$calcFlattened,
        anisoRadius = self$funParams()$anisoRadius,
        preDilationSize = self$funParams()$preDilationSize,
        postDilationSize = self$funParams()$postDilationSize,
        saveMeshes = if ("saveMeshes" %in% names(self$funParams())) self$funParams()$saveMeshes else FALSE
      )
      
      # call python
      self$pyScript("create_branching", params)
      
      # DONE
      self$writeLog("Done branching")
      # self$exitLog()
      self$writeLog("Update image information")
      
      # update image information
      cciaObj$setImLabelsFilepath(
        paste0(branchingName, cciaConf()$files$ext$labels),
        valueName = branchingName, setDefault = FALSE
      )
      
      # load data from disk when loading the object
      # instead of pushing into object which makes it big
      # and slow to load
      cciaObj$setImLabelPropsFilepath(
        paste0(branchingName, cciaConf()$files$ext$labelProps),
        valueName = branchingName, setDefault = FALSE
      )
      
      # create populations for branching
      popType <- "branch"
      
      self$writeLog("Create population DT")
      
      # create populations for branching types
      # create classification populations
      popDT <- cciaObj$popDT(popType, pops = c(branchingName),
                             popCols = c("branch-type"))
      
      # add children
      pops <- list()
      parentPops <- branchingName
      
      for (i in unique(popDT$`branch-type`)) {
        pops[[xfun::numbers_to_words(i)]] <- list(
          filterMeasure = "branch-type",
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
                              valueName = branchingName)
      
      # save to disk
      cciaObj$savePops(popType, purge = TRUE, includeFiltered = TRUE)
      
      # save object
      cciaObj$saveState()
      
      self$writeLog("Done Populations")
    }
  )
)
