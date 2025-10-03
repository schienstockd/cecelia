AssignToPops <- R6::R6Class(
  "AssignToPops",
  inherit = Objcl,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "assignToPops",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Assign classified objects to populations")
      
      # save pops before call
      cciaObj <- self$cciaTaskObject()
      
      # split population types off pops
      # TODO this should be done once working for more value names
      # popTypes <- popTypesFromPops(self$funParams()$pops)
      
      # save
      for (x in c(self$funParams()$popType, "clsf")) {
        cciaObj$savePopMap(x, includeFiltered = TRUE)
        cciaObj$savePops(x, purge = TRUE, includeFiltered = TRUE)
      }
      
      # prepare params
      params <- list(
        taskDir = self$envParams()$dirs$task,
        imPath = cciaObj$imFilepath(),
        popLabelPaths = lapply(
          cciaObj$valueNames("imLabelsFilepath", valueType = "nonClsf"),
          function(x) cciaObj$imLabelsFilepath(x)
        ),
        clsfLabelPaths = lapply(
          cciaObj$valueNames("imLabelsFilepath", valueType = "clsf"),
          function(x) cciaObj$imLabelsFilepath(x)
        ),
        pops = self$funParams()$pops,
        clsfPops = self$funParams()$clsfPops,
        popType = self$funParams()$popType,
        valueName = self$funParams()$valueName,
        minClsfValue = self$funParams()$minClsfValue,
        assignMethod = self$funParams()$assignMethod
      )
      
      # call python
      self$pyScript("assign_to_pops", params)
      
      # add classification populations
      if (self$funParams()$createPops == TRUE) {
        popType <- self$funParams()$popType
        for (i in self$funParams()$pops) {
          pops <- list()
          parentPops <- i
          
          # go through classifier pops
          for (j in self$funParams()$clsfPops) {
            pops[[sprintf("pos.%s", j)]] <- list(
              filterMeasure = sprintf(
                "%s.cell.cl.bool#clsf.%s",
                popType, j
              ),
              filterValues = TRUE,
              filterFun = "eq"
            )
            # pops[[sprintf("neg.%s", j)]] <- list(
            #   filterMeasure = sprintf(
            #     "%s.cell.cl.bool#clsf.%s",
            #     popType, j
            #   ),
            #   filterValues = FALSE,
            #   filterFun = "eq",
            #   filterDefaultAll = TRUE
            # )
          }
          
          # set names
          # names(pops) <- sprintf("%s.%s", c("pos", "neg"), stringr::str_replace(j, "/", "__"))
          names(pops) <- sprintf("%s.%s", c("pos"), stringr::str_replace(j, "/", "__"))
          
          # remove populations
          cciaObj$delPopsByPath(
            popType,
            pops = levels(interaction(parentPops, names(pops), sep = "/")),
            includeFiltered = TRUE
          )
          
          # add populations
          cciaObj$addFilteredPops(popType, parentPops, pops,
                                  valueName = self$funParams()$valueName)
        }
        
        # save to disk
        cciaObj$savePops(popType, purge = TRUE, includeFiltered = TRUE)
        cciaObj$saveState()
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
