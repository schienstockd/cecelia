#' Population utility that combine multiple label properties into one
#' population interface. I only used this for live cell tracking so far where
#' every base population is a single adata-file.
#' 
#' @name MultifileLabelPopUtils
#' @description Multi file label populations
#'
#' @examples
#' TODO
#' @export
MultifileLabelPopUtils <- R6::R6Class(
  "MultifileLabelPopUtils",
  inherit = PopulationUtils,
  
  ## private
  private = list(
    # setters
    
    # getters
  ),
  
  ### public
  public = list(
    #' @description Init
    #' @param taskDir character for task directory
    #' @param valueNames list of character for value names
    #' @param imChannels list of character for channel names
    initialize = function(taskDir, valueNames, imChannels) {
      super$initialize(valueNames, imChannels)
      
      # init label utils
      lpu <- list()
      for (x in valueNames) {
        # add to list
        lpu[[x]] <- cciaEnv()$LabelPropsUtils(taskDir, value_name = x)
      }
      
      # set value names
      private$setValueNames(valueNames, invalidate = FALSE)
      
      # set object
      private$setPopObj(lpu, invalidate = FALSE)
      
      # init channel limits
      # private$initImChannelLimits()
    },
    
    #' @description default pops
    defaultPops = function() {
      names(self$getPopObj())
    },
    
    #' @description Population data.table
    #' @param pops list of character for populations
    #' @param popCols list of character for columns
    #' @param dropNA boolean to drop NA
    #' @param dropPop boolean to drop population
    #' @param copyDT boolean to copy data.table
    popDT = function(pops = NULL, popCols = NULL, dropNA = FALSE, dropPop = FALSE) {
      # set names if not provided or 'root' is given
      if (is.null(pops) || .flowPopIsRoot(pops)) {
        pops <- names(self$getPopObj())
      }
      
      # build pop list
      popList <- list()
      
      # go through label prop utils and build datatable
      for (x in pops[pops != "root"]) {
        labelsView <- self$getPopObj()[[x]]$label_props_view()
        
        if (!is.null(labelsView)) {
          # filter on columns?
          if (!is.null(popCols)) {
            # always add label if not present
            if (!"label" %in% popCols)
              popCols <- c("label", popCols)
            
            # centroids
            if ("centroids" %in% popCols) {
              popCols <- popCols[popCols != "centroids"]
              popCols <- c(
                labelsView$centroid_columns(),
                popCols
                )
            }
            
            # show popCols
            if (length(popCols) > 0) {
              labelsView$view_cols(popCols)
            }
          }
          
          popList[[x]] <- as.data.table(
            labelsView$change_channel_names(self$getImChannels())$as_df()
            )
          
          # drop NA?
          if (dropNA == TRUE) {
            # popList[[x]] <- popList[[x]] %>% drop_na()
            # https://stackoverflow.com/a/14363380/13766165
            popList[[x]] <- popList[[x]][complete.cases(popList[[x]]),]
          }
          
          # add pop name
          # in this case, value name and pop are the same,
          # however, for flow gating they might have the same
          # value name but a different population name
          if (dropPop == FALSE) {
            popList[[x]][, pop := x]
          }
        }
      }
      
      DT <- data.table::rbindlist(popList, idcol = TRUE, fill = TRUE)
      
      # rename populations
      if (nrow(DT) > 0) {
        setnames(DT, ".id", "value_name")
      }
      
      DT
    },
    
    #' @description Save anndata back to disk
    save = function() {
      # go through label views and save
      for (x in self$getPopObj()) {
        if (!is.null(x$label_props_view()))
          x$label_props_view()$save()
      }
    },
    
    #' @description Populations paths
    #' @param includeRoot boolean to include root
    popPaths = function(includeRoot = FALSE) {
      if (includeRoot == TRUE)
        return(c("root", names(self$getPopObj())))
      else
        return(names(self$getPopObj()))
    },
    
    #' @description Pop labels
    popLabels = function() {
      self$popDT()[, c("pop", "label")]
    }
    
    ## setters
    
    ## getters
  )
)
