NetworkWeights <- R6::R6Class(
  "NetworkWeights",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "networkWeights",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Calculate network weights")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # get DTs
      branchDT <- cciaObj$popDT(
        "branch", includeFiltered = TRUE, completeDT = TRUE,
        completeValueNames = self$funParams()$valueName,
        pops = unlist(unname(cciaObj$popPaths(
          "branch", includeFiltered = TRUE, includeRoot = FALSE))))
      
      # this is designed to return weights based on tracked cell interactions
      # so, these populations should be from live?
      # unless you coul infer the weight of a network based on
      # how many cells there are in the vicinity of a given branch
      popDT <- cciaObj$popDT(
        self$funParams()$popType, includeFiltered = TRUE, pops = self$funParams()$pops)
      
      # now build a weighted graph of the most visited edges and save back to branchDT
      # then you can use that to display on the image
      if (cciaObj$omeXMLPixels()$SizeZ > 1)
        centroidCols <- c('centroid_z', 'centroid_y', 'centroid_x')
      else
        centroidCols <- c('centroid_y', 'centroid_x')
        
      x <- as.matrix(branchDT[, ..centroidCols])
      rownames(x) <- branchDT$label
      
      # find nearest neighbours
      popNN <- dbscan::kNN(x, k = 1, query = as.matrix(popDT[, ..centroidCols]))
      
      # filter long distances
      # regionVals <- regionDT[popNN$id, ]$regions
      # regionVals <- branchDT[popNN$id, ]$`branch-type`
      popDT[, branch.id := popNN$id]
      popDT[, branch.dist := popNN$dist]
      popDT[branchDT[, c("label", "branch-type")], on = c("branch.id" = "label"),
            `branch-type` := `branch-type`]

      # get frequency of cells close to branches
      branchDT[, weight := 0]
      
      # branchWeights <- unique(popDT[branch.dist <= maxDist, c("branch.id", "track_id")]) %>%
      # branchWeights <- popDT[branch.dist <= self$funParams()$maxDist, c("branch.id", "track_id")] %>%
      branchWeights <- popDT[branch.dist <= self$funParams()$maxDist, c("branch.id", "label")] %>%
        dplyr::group_by(branch.id) %>%
        summarise(n = n())
      
      branchDT[
        # label %in% unique(popDT[branch.dist < self$funParams()$maxDist]$branch.id),
        label %in% branchWeights$branch.id,
        # weight := table(popDT[branch.dist < self$funParams()$maxDist, c("branch.id")])]
        weight := branchWeights$n]

      # weightUp <- quantile(branchDT$weight, self$funParams()$upperPercentile/100)
      # branchDT[weight > weightUp, weight := weightUp] 
      
      # create a column and add to label props
      cciaObj$labelProps(valueName = paste0(self$funParams()$valueName, ".branch"))$add_obs(
        reticulate::r_to_py(list("branch-weight" = branchDT$weight))
      )$save()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      # self$updateImageInfo()
      
      TRUE
    }
  )
)
