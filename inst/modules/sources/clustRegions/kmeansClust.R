KmeansClust <- R6::R6Class(
  "KmeansClust",
  inherit = ClustRegions,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "kmeansClust",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start KMeans clustering")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Run clustering")
      
      # get uIDs
      uIDs <- NULL
      if ("uIDs" %in% names(self$funParams()))
        uIDs <- self$funParams()$uIDs
      
      # define value name
      valueName <- self$funParams()$valueName
      popType <- self$funParams()$popType
      
      self$writeLog(valueName)
      self$writeLog(uIDs)
      
      # get spatial DT
      # TODO make value name selectable
      spatialDT <- cciaObj$spatialDT(valueName = valueName, uIDs = uIDs)
      
      # filter same type associations
      if (self$funParams()$excludeSelfConnections == TRUE)
        spatialDT <- spatialDT[clusters.to != clusters.from]
      
      # expand pops?
      pops <- self$funParams()$pops
      if (self$funParams()$expandPops == TRUE) {
        # get all pops
        # TODO is there a better way?
        pops <- unique(unlist(unname(lapply(cciaObj$cciaObjects(uIDs = uIDs), function(x) {
          unlist(sapply(self$funParams()$pops, function(y) {
            x$popLeaves(self$funParams()$popType, pop = y)
          }))
        }))))
      }
      
      # get populations
      popDT <- cciaObj$popDT(
        popType = popType,
        pops = pops,
        includeFiltered = TRUE,
        uIDs = uIDs)
      
      # join pops
      groupFrom <- "pop.from"
      groupTo <- "pop.to"
      
      if (self$funParams()$popType == "clust" && self$funParams()$useClusters == TRUE) {
        spatialDT[popDT[, c("uID", "label", "clusters")],
                  on = c("uID", "to" = "label"),
                  clusters.to := clusters]
        spatialDT[popDT[, c("uID", "label", "clusters")],
                  on = c("uID", "from" = "label"),
                  clusters.from := clusters]
        
        groupFrom <- "clusters.from"
        groupTo <- "clusters.to"
      } else {
        spatialDT[popDT[, c("uID", "label", "pop")],
                  on = c("uID", "to" = "label"),
                  pop.to := pop]
        spatialDT[popDT[, c("uID", "label", "pop")],
                  on = c("uID", "from" = "label"),
                  pop.from := pop]
      }
      
      # get "i-niches" and determine a given number of defined clusters
      freqRegions <- spatialDT %>%
        group_by(.dots = c("uID", "from", groupFrom, groupTo)) %>%
        summarise(n = n()) %>%
        mutate(freq = n/sum(n) * 100) %>%
        drop_na() %>%
        # TODO do I need this .. ?
        ungroup() %>%
        complete(uID, !!!syms(groupFrom), !!!syms(groupTo), fill = list(freq = 0))
      
      # use K-Means to get clusters of regions
      # normalit <- function(m) (m - min(m))/(max(m)-min(m))
      
      # include counts?
      if (self$funParams()$includeCounts == TRUE) {
        freqRegionsWider <- as.data.table(
          freqRegions %>% pivot_wider(
            id_cols = c(uID, from, sym(groupFrom)),
            names_from = sym(groupTo),
            values_from = c(freq, n),
            values_fill = 0))
      } else {
        freqRegionsWider <- as.data.table(
          freqRegions %>% pivot_wider(
            id_cols = c(uID, from, sym(groupFrom)),
            names_from = sym(groupTo),
            values_from = freq,
            values_fill = 0))
      }
      
      # add total count?
      if (self$funParams()$includeTotalCount > 0) {
        freqRegionsWider <- freqRegionsWider[
          as.data.table(spatialDT %>%
            group_by(.dots = c("uID", "from")) %>%
            summarise(sum = sum(n()))),
          on = c("uID", "from")]
      }
      
      # add object measurements?
      if (length(self$funParams()$objectMeasures) > 0) {
        popCols <- c("uID", "label", self$funParams()$objectMeasures)
        
        # TODO can you do this by reference assignment?
        spatialDT <- spatialDT[popDT[, ..popCols], on = c("uID", "to" = "label")]
        
        # add to regions
        freqRegionsWider <- freqRegionsWider[
          as.data.table(spatialDT %>%
            group_by(.dots = c("uID", "from")) %>%
            summarise(across(self$funParams()$objectMeasures, ~ mean(.x, na.rm = TRUE)))),
          on = c("uID", "from")]
      }
      
      # drop na
      freqRegionsWider <- freqRegionsWider[complete.cases(freqRegionsWider)]
      
      freqRegionsMat <- as.matrix(
        freqRegionsWider %>%
          ungroup() %>% 
          select(-c(uID, from, sym(groupFrom))))
      
      rownames(freqRegionsMat) <- freqRegionsWider$from
      
      exclCols <- colnames(freqRegionsWider)[colnames(freqRegionsWider) %in% c(
        "uID", "from", "regions")]
      
      clusters <- kmeans(
        freqRegionsWider %>%
          ungroup() %>%
          select(-c(exclCols, sym(groupFrom))),
        self$funParams()$numClusters)
      # mcl.model <- mclust::Mclust(freqRegionsMat)
      # mcl.model <- mclust::Mclust(freqRegionsMat, clustToFind)
      
      # push back to label properties
      freqRegionsWider$regions <- clusters$cluster
      
      # rename from to label
      freqRegionsWider <- freqRegionsWider %>%
        dplyr::rename(label = from)
      
      # create path
      regionsFilename <- paste0(valueName, ".region", cciaConf()$files$ext$labelProps)
      
      # go through objects
      for (x in cciaObj$cciaObjects(uIDs = uIDs)) {
        # get data
        y <- freqRegionsWider[uID == x$getUID()]
        
        self$writeLog(sprintf("save %s", x$getUID()))
        
        # exclude columns
        exclCols <- colnames(y)[colnames(y) %in% c("uID", "pop.from", "clusters.from")]
        
        # save as Anndata
        cciaEnv()$LabelPropsUtils(
          # self$envParams()$dirs$task,
          x$persistentObjectDirectory(),
          file.path(cciaConf()$dirs$tasks$labelProps, regionsFilename)
        )$label_props(
          reticulate::r_to_py(y %>% dplyr::select(-exclCols)),
          save = reticulate::r_to_py(TRUE),
          obs_cols = reticulate::r_to_py(list(
            "label", "regions"
          ))
        )
        
        # set path
        x$setImRegionsFilepath(regionsFilename, valueName = valueName)
        x$saveState()
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
    }
  )
)
