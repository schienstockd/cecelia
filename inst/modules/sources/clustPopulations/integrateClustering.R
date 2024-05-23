IntegrateClustering <- R6::R6Class(
  "IntegrateClustering",
  inherit = ClustPopulations,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "integrateClustering",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start integrating clustering")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get uIDs
      uIDs <- NULL
      if ("uIDs" %in% names(self$funParams()))
        uIDs <- self$funParams()$uIDs
      
      # define value name
      valueName <- self$funParams()$valueName
      popType <- self$funParams()$popType
      # pops <- self$funParams()$pops
      pops <- c("root")
      prepClusterNum <- self$funParams()$prepClusterNum
      
      self$writeLog(valueName)
      self$writeLog(uIDs)
      
      # get populations
      popDT <- cciaObj$popDT(
        popType = popType,
        pops = pops,
        includeFiltered = TRUE,
        uIDs = uIDs)
      
      # get matrix
      popMat <- t(adataMatFromPopDT(popDT[
        , -c("clusters.group", "label", "pop", paste0("centroid_", c("x", "y", "z", "t")))],
        popKeys = c("uID", "clusters")))
      
      # run UMAP
      # TODO this is a bit arbitrary ...
      popUMAP <- umap::umap(popMat)
      umapDF <- as.data.frame(popUMAP$layout) %>%
        rownames_to_column() %>%
        separate_wider_delim(cols = rowname, delim = ".", names = c("uID", "clusters"))
      
      # run KMeans to get the same number of clusters?
      clustToFind <- self$funParams()$numClusters
      
      if (prepClusterNum == "max")
        clustToFind <- max(as.numeric(umapDF$clusters))
      else if (prepClusterNum == "median")
        clustToFind <- median((umapDF %>% group_by(uID) %>% summarise(max.group = max(as.numeric(as.character(clusters)))))$max.group)
      else if (prepClusterNum == "min")
        clustToFind <- min((umapDF %>% group_by(uID) %>% summarise(max.group = max(as.numeric(as.character(clusters)))))$max.group)
      
      # get clusters with KMeans
      # TODO try other algorithms - although this seem to be ok for now
      # clusters <- kmeans(popMat, clustToFind)
      # umapDF$clusters.group <- as.factor(clusters$cluster)
      
      # https://mclust-org.github.io/mclust/articles/mclust.html
      library(mclust)
      BIC <- mclustBIC(popMat, clustToFind)
      mod1 <- Mclust(popMat, x = BIC)
      umapDF$clusters.group <- as.factor(mod1$classification)
      
      # TODO not sure about this
      # find clusters on UMAP
      # clusters <- dbscan::dbscan(
      #   umapDF[, c("V1", "V2")], eps = 0.4, minPts = 3, borderPoints = TRUE)
      # umapDF$clusters.group <- as.factor(clusters$cluster)
      
      # TODO not cool
      saveRDS(umapDF, file = file.path(
        cciaObj$persistentObjectDirectory(), "data", paste0(valueName, ".cluster.groups.rds")))
      
      # push cluster groups back to DF
      popDT[, clusters.group := NULL]
      for (i in rownames(umapDF)) {
        x <- umapDF[i, ]
        
        popDT[uID == x$uID & clusters == x$clusters, clusters.group := as.numeric(x$clusters.group)]
      }
      
      # create path
      labelsFilename <- paste0(valueName, ".clust", cciaConf()$files$ext$labelProps)
      obsCols <- c("clusters.group")
      
      # go through objects
      for (x in cciaObj$cciaObjects(uIDs = uIDs)) {
        # get data
        y <- popDT[uID == x$getUID()]
        
        self$writeLog(sprintf("save %s", x$getUID()))
        
        labels <- cciaEnv()$LabelPropsUtils(
          task_dir = x$persistentObjectDirectory(),
          labels_file = file.path("labelProps", "default.clust.h5ad")
        )$label_props_view()
        
        if (!is.null(labels)) {
          # extend root by labels if size does not match
          # labelsDT <- as.data.table(labels$view_label_col()$values_obs())
          labelsDT <- as.data.table(labels$values_obs())
          
          # TODO is there an in memory version for that?
          # rootDT <- labelsDT[rootDT[, c("label", obsCols)], on = c("label")]
          y <- y[labelsDT, on = c("label")]
          
          # order DT
          # TODO this should not be necessary?
          # This might be due to multi-layered segmentation merged?
          y[, label := factor(label, levels = labels$values_obs()$label)]
          setkey(y, label)
          
          labels$add_obs(reticulate::r_to_py(as.list(y[, ..obsCols])))
          
          # save and close
          labels$save(revert_channel_names = FALSE)
          labels$close()
        }
        
        # save
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
