#' Utils for Anndata
#' 
#' @name AnndataUtils
#' @description Utils for Anndata
#'
#' @examples
#' TODO
#' @export
AnndataUtils <- R6::R6Class(
  "AnndataUtils",
  inherit = PopulationUtils,
  
  ### private
  private = list(
    adataDT = NULL,
    
    ## setters
    setAdataDT = function(x, invalidate = TRUE) {
      private$adataDT <- x
      private$invalidate(invalidate = invalidate)
    }
    
    ## getters
  ),
  
  ### public
  public = list(
    #' @description init
    #' @param objFilepath character of object file path
    #' @param imChannels list of character for channel names
    initialize = function(objFilepath, imChannels) {
      super$initialize(objFilepath, imChannels)
      
      # get task dir and labels file from filepath
      labelsFile <- stringr::str_extract(objFilepath, file.path(
        cciaConf()$dirs$tasks$labelProps, paste0(".*", cciaConf()$files$ext$labelProps)
        ))
      taskDir <- stringr::str_replace(objFilepath, labelsFile, "")
      
      # load label props
      labelsView <- cciaEnv()$LabelPropsUtils(
        task_dir = taskDir,
        labels_file = labelsFile
      )$label_props_view()
      
      # change channel names
      # labelsView$change_channel_names(
      #   .flowCorrectChannelNames(self$getImChannels()[usedChannels]))
      
      # get label DT
      labelsDT <- as.data.table(labelsView$as_df())
      labelsView$close()
      
      # get channels
      usedChannels <- names(labelsDT)[
        !is.na(c(stringr::str_match(names(labelsDT), "mean_intensity")))
      ]
      
      # go through and rename
      for (x in usedChannels) {
        # is there a prefix?
        columnPrefix <- as.character(stringr::str_match(x, ".+(?=_mean_intensity)"))
        
        if (is.na(columnPrefix))
          setnames(labelsDT, x,
                   self$getImChannels()[[as.numeric(stringr::str_match(x, "[0-9]+")) + 1]])
        else
          setnames(labelsDT, x,
                   paste(columnPrefix, self$getImChannels()[[as.numeric(stringr::str_match(x, "[0-9]+")) + 1]], sep = "_"))
      }
      
      # add root as population
      labelsDT$pop <- "root"
      
      # convert to DT
      private$setAdataDT(labelsDT, invalidate = FALSE)
      
      # init channel limits
      # private$initImChannelLimits()
      
      # set value name
      # TODO this assumes default
      private$setValueNames(c("default"), invalidate = FALSE)
    },
    
    #' @description Population data.table
    #' @param pops list of character for populations
    #' @param popCols list of character for columns
    #' @param dropNA boolean to drop NA
    #' @param dropPop boolean to drop population
    #' @param copyDT boolean to copy data.table
    popDT = function(pops = NULL, popCols = NULL, dropNA = FALSE, dropPop = FALSE, copyDT = TRUE) {
      # TODO that should not change?
      # no pops will be selected here
      # so, just return the same DT every time
      # populations will be filtered?
      DT <- self$getAdataDT()
      
      # filter on cols
      if (!is.null(popCols)) {
        return(DT[, ..popCols])
      } else {
        if (copyDT == TRUE)
          return(copy(DT))
        else
          return(DT)
      }
    },
    
    #' @description Channel matrix
    adataMat = function() {
      anndataMat <- NULL
      
      if (!is.null(self$popDT())) {
        # convert channels to matrix
        anndataMat <- adataMatFromPopDT(self$popDT())
      }
      
      anndataMat
    },
   
    #' @description Save anndata back to disk
    save = function() {
      # nothing to save
    },
    
    #' @description Populations paths
    #' @param includeRoot boolean to include root
    popPaths = function(includeRoot = FALSE) {
      # There are no populations saved in adata itself
      # except root!
      if (includeRoot == TRUE)
        c("root")
      else
        list()
    },
    
    #' @description Whether utils are based on label props
    #' for clusters, pull information from the main DT
    isLabelPropsStore = function() {
      FALSE
    },
    
    ## setters
    
    ## getters
    getAdataDT = function() {
      private$adataDT
    }
  )
)
