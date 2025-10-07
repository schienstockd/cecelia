#' @description Correct channel names
#' @param channelNames list of character for channel names
#' @examples
#' TODO
#' @export
.flowCorrectChannelNames <- function(channelNames) {
  channelNames <- gsub(" |\\(|\\)|\\-", ".", channelNames)
  channelNames <- gsub("\\.+", ".", channelNames)
  channelNames <- gsub("\\.$", "", channelNames)
  
  channelNames
}

#' @description Generate colours for flow plots
#' @param x list of numeric for 'X'-values
#' @param y list of numeric for 'Y'-values
#' @param colramp color palette
#' @examples
#' TODO
#' @export
.flowColours <- function(x, y, nbin = 128, colramp = NULL) {
  retVal <- NULL
  
  if (is.null(colramp))
    colramp <- flowViz::flowViz.par.get("argcolramp")
  
  if (all(length(x) > 0, length(y) > 0)) {
    retVal <- densCols(x, y, nbin = nbin, colramp = colramp)
  }
  
  retVal
}

#' @description Generate contour lines
#' @param DT data.table for population
#' @param xCol character for 'X'-column
#' @param yCol character for 'Y'-column
#' @param n integer for number of bins
#' @param n integer for bandwith of bins
#' @seealso MASS::kde2d
#' @param confidenceLevels list of numeric for confidence intervals
#' @param extendLimits numeric to extend limits and avoid "cutting off" contours
#' @examples
#' TODO
#' @export
.flowContourLines <- function(
    DT, xCol, yCol, dens = NULL, n = 25, confidenceLevels = c(0.95, 0.90, 0.75, 0.5),
    extendLimits = 0.5, pointsInContour = TRUE) {
  if (is.null(dens)) {
    # get density
    dens <- MASS::kde2d(
      DT[[xCol]], DT[[yCol]], n = n,
      lims = c(
        range(DT[[xCol]]) * c(1 - extendLimits, 1 + extendLimits),
        range(DT[[yCol]]) * c(1 - extendLimits, 1 + extendLimits)
        ))
    
    # get z range
    zRange <- range(dens$z)
    
    # normalise z
    dens$z <- (dens$z - zRange[[1]]) / (zRange[[2]] - zRange[[1]])
  }
  
  # create contour levels
  contourLevels <- 1 - confidenceLevels
  confidenceLines <- contourLines(dens, levels = contourLevels)
  
  # create dataframe for lines
  confidenceDT <- data.table::rbindlist(
    mapply(function(x, i) {
      as.data.table(
        list(
          level = x$level,
          seq = i,
          x = x$x,
          y = x$y
        ))
    }, confidenceLines, seq(length(confidenceLines)),
    SIMPLIFY = FALSE)
  )
  
  if (pointsInContour == TRUE) {
    # determine points in contour
    # get larges contour
    DT[, in_contour := 0]
    
    # go through contours
    for (i in seq(length(confidenceLines))) {
      x <- confidenceDT[seq == i,]
      
      DT[in_contour == 0,
         in_contour := sp::point.in.polygon(
           DT[in_contour == 0, get(xCol)],
           DT[in_contour == 0, get(yCol)],
           x$x, x$y
           )]
    }
  }
  
  confidenceDT
}

#' @description Prepare flowFrame
#' @param x data.table or data.frame for population
#' @param channelNames list of character for channel names
#' @param attrNames list of character for attribute names
#' @param channelPattern character for channel pattern to extract
#' @param addRownames boolean to add rownames
#' @param flowNames boolean to use corrected "flow-names"
#' @examples
#' TODO
#' @export
.prepareFlowFrame <- function(
    x, channelNames, attrNames = NULL,
    channelPattern = "mean_intensity", addRownames = FALSE,
    flowNames = TRUE) {
  channelDF <- NULL
  # only use channels
  if (!is.null(channelPattern)) {
    channelDF <- x %>%
      dplyr::select(contains(channelPattern))
    
    # correct channel names before selecting
    if (flowNames == TRUE) {
      # correct channel names
      channelNames <- .flowCorrectChannelNames(channelNames)
    }
    
    # if channel DF is empty, the channels might have
    # already been renamed
    if (ncol(channelDF) == 0) {
      # make sure all channel names are in DF
      channelNames <- unlist(channelNames)
      channelNames <- channelNames[channelNames %in% colnames(x)]
      
      channelDF <- x %>%
        dplyr::select(all_of(channelNames))
    }
    
    # rename channels
    colnames(channelDF) <- .flowCorrectChannelNames(channelNames)
  }
  
  attrDF <- NULL
  # get attributes
  if (!is.null(attrNames)) {
    attrDF <- x %>%
      dplyr::select(all_of(attrNames[attrNames %in% colnames(x)]))
  }
  
  # combine and replace Nan
  x <- cbind(channelDF, attrDF)
  
  # replace NaN values
  x[is.na(x >= 0)] <- 0
  
  # add rownames
  if (addRownames == TRUE) {
    x <- x %>%
      tibble::rownames_to_column() %>%
      dplyr::mutate(across(where(is.character), as.numeric))
  }
  
  # # add metadata
  # metadata <- list(
  #   name = dimnames(x)[[2]],
  #   desc = paste(dimnames(x)[[2]]))
  # 
  # # Create FCS file metadata - ranges, min, and max settings
  # metadata$minRange <- apply(x, 2, min)
  # metadata$maxRange <- apply(x, 2, max)
  # 
  # # create flowframe
  # x.ff <- new(
  #   "flowFrame",
  #   exprs = as.matrix(x),
  #   description = metadata
  #   )
  # 
  # # save FCS
  # write.FCS(x.ff, fcsOut, what = "double")
  
  # https://github.com/RGLab/cytolib/issues/54#issuecomment-1175529424
  flowCore::flowFrame(as.matrix(x))
}

#' @description Create gating set
#' @param ffs list of FlowFrames to use 
#' @param channelNames list of character for channel names
#' @param ffNames list of character for FlowFrame names
#' @param transformation character for data transformation. Any of c("none", "biexponential", "log", "ln", "linear", "quadratic", "scale", "splitScale", "truncate")
#' @param flowNames boolean to use "flow-names"
#' @examples
#' TODO
#' @export
.flowCreateGatingSet <- function(ffs, channelNames = list(), ffNames = NULL,
                                transformation = NULL, flowNames = TRUE) {
  # fs <- read.ncdfFlowSet(fcsFiles, alter.names = TRUE)
  # fs <- read.FCS(fcsFiles)
  # fs <- read.flowSet(fcsFiles, alter.names = TRUE)
  # cs <- load_cytoset_from_fcs(fcsFiles, alter.names = TRUE)
  
  # get cyto set
  # https://github.com/RGLab/cytolib/issues/54#issuecomment-1175529424
  if (!is.list(ffs))
    ffs <- list(ffs)
  if (!is.null(ffNames))
    names(ffs) <- ffNames
  
  # transform flowFrame
  fsTrans <- .flowTransformFlowSet(
    as(ffs, "flowSet"), channelNames, transformation = transformation,
    flowNames = flowNames)
  
  # create gating set
  flowWorkspace::GatingSet(fsTrans)
}

#' @description Transform flow set
#' @param fs FlowSet
#' @param channelNames list of character for channel names
#' @param transformation character for data transformation. Any of c("none", "biexponential", "log", "ln", "linear", "quadratic", "scale", "splitScale", "truncate")
#' @param flowNames boolean to use "flow-names"
#' @examples
#' TODO
#' @export
.flowTransformFlowSet <- function(fs, channelNames, transformation = NULL,
                                 flowNames = TRUE) {
  # run transformation
  if (!is.null(transformation) && transformation != "none") {
    if (transformation == "biexponential") transFun <- flowCore::biexponentialTransform()
    else if (transformation == "log") transFun <- flowCore::logTransform()
    else if (transformation == "ln") transFun <- flowCore::lnTransform()
    else if (transformation == "linear") transFun <- flowCore::linearTransform()
    else if (transformation == "quadratic") transFun <- flowCore::quadraticTransform()
    else if (transformation == "scale") transFun <- flowCore::scaleTransform()
    else if (transformation == "splitScale") transFun <- flowCore::splitScaleTransform()
    else if (transformation == "truncate") transFun <- flowCore::truncateTransform()
    else if (transformation == "logicle") transFun <- flowCore::logicleTransform()
    
    # correct channel names
    if (flowNames == TRUE) {
      channelNames <- .flowCorrectChannelNames(channelNames)
    }
    
    transList <- flowCore::transformList(channelNames, transFun)
    
    return(flowCore::transform(fs, transList))
  } else {
    return(fs)
  }
}

#' @description Apply compensation to GatingSet
#' @param gs GatingSet
#' @param compMat matrix for compensation
#' @examples
#' TODO
#' @export
.flowCompensateGs <- function(gs, compMat) {
  flowCore::compensate(gs, compMat)
}

#' @description Fortify gating set
#' @importFrom ggplot2 fortify
#' 
#' @param gs GatingSet
#' @param subset character of population subset
#' @examples
#' TODO
#' @export
.flowFortifyGs <- function(gs, cols = NULL, subset = "root") {
  retVal <- NULL
  
  tryCatch(
    expr = {
      # fortify gating set
      # make sure the population is in the gating set
      if (subset %in% flowWorkspace::gs_get_pop_paths(gs)) {
        attr(gs, "subset") <- subset
        
        if (!is.null(cols)) {
          colsGs <- flowWorkspace::colnames(gs)
          cols <- colsGs[colsGs %in% cols]
          
          # make sure the columns are in the gatingset
          if (length(cols) > 0)
            attr(gs, "dims") <- data.table(name = cols)
        }
        
        # retVal <- fortify(gs)
        retVal <- ggcyto:::fortify.GatingSet(gs)
      }
    },
    error = function(e) {
      message(e)
      retVal <<- NULL
    },
    warning = function(w) {
      warning(e)
      retVal <<- NULL
    }
  )
  
  retVal
}

# # auto gate on all channels
# flowAutoGate <- function(gs, channelNames) {
#   # create a flow set
#   fs <- read.ncdfFlowSet(file.path(taskDir, "labelIntensities.fcs"), alter.names = TRUE)
#   chnlNames <- names(intensities)[names(intensities) != "rowname"]
#   
#   # TODO .. how should that work?
# }

#' @description Add polygon gate
#' @param gateCoords list of (N,2) gate coordinates
#' @param x character of 'X'-coordinate
#' @param y character of 'Y'-coordinate
#' @examples
#' TODO
#' @export
.flowPolygonGate = function(gateCoords, x, y) {
  # create matrix
  # https://stackoverflow.com/a/43425453/13766165
  # transpose?
  if (!is.null(attr(gateCoords, "class")) && nrow(gateCoords) > ncol(gateCoords))
    mat <- do.call(cbind, gateCoords)
  else
    mat <- t(do.call(cbind, gateCoords))
  colnames(mat) <- c(x, y)
  
  # make new polygon gate
  pg1 <- flowCore::polygonGate(.gate = mat)
  
  list(pg1)
}

#' @description Compensate with linear model
#' @param df data.frame to compensate
#' @param channelNames list of character for channel names
#' @param refAxis character for reference axis
#' @param polyDegree integer for plynomial degress
#' @param suffix character for column suffix
#' @param replaceValues boolean to replace previous values
#' @param flowNames boolean to use "flow-names"
#' @param cropDataBySD boolean to crop data by SD
#' @examples
#' TODO
#' @export
.flowCompensatePoly <- function(df, channelNames, refAxis,
                               polyDegree = 4, suffix = ".corr",
                               replaceValues = FALSE, flowNames = TRUE,
                               cropDataBySD = FALSE) {
  # correct channel names before selecting
  if (flowNames == TRUE) {
    # correct channel names
    channelNames <- .flowCorrectChannelNames(channelNames)
  }
  
  # correct all channels
  for (x in channelNames) {
    # get sd to limit data
    medianIntensity <- median(df[[x]], na.rm = TRUE)
    
    if (cropDataBySD == TRUE) {
      # TODO is there a better way - this is a bit arbitrary
      dfSD <- sd(df[[x]], na.rm = TRUE) * 2
      dataDF <- df[df[[x]] >= medianIntensity - dfSD & df[[x]] <= medianIntensity + dfSD,]
    } else {
      dataDF <- copy(df)
    }
    
    # fit model
    # https://stackoverflow.com/a/3822706/13766165
    polyEstimate <- lm(get(x) ~ poly(get(refAxis), polyDegree, raw = FALSE), data = dataDF)
    
    # predict values
    polyPredict <- predict(
      polyEstimate,
      newdata = data.frame(x = df[[refAxis]]) %>%
        rename_with(.cols = 1, ~refAxis))
    
    # correct values to median
    polyCorrected <- polyPredict / medianIntensity
    
    # set name for correction
    corrName <- if (replaceValues)
      x
    else
      paste0(x, suffix)
    
    # correct values
    df[[corrName]] <- df[[x]] / polyCorrected
  }
  
  df
}

#' @description Get gate for pop
#' @param gs GatingSet
#' @param pop character for population
#' @examples
#' TODO
#' @export
.flowGateForPop <- function(gs, pop) {
  # there will only be one gate returned
  flowWorkspace::gs_pop_get_gate(gs, pop)[[1]]
}

#' @description Get stats for pop
#' @param gs GatingSet
#' @param pop character for population
#' @param ... passed to gs_pop_get_stats
#' @examples
#' TODO
#' @export
.flowStatsForPop <- function(gs, pop, ...) {
  flowWorkspace::gs_pop_get_stats(gs, pop, ...)
}

#' @description Get channels for pop
#' @param gs GatingSet
#' @param pop character for population
#' @examples
#' TODO
#' @export
.flowChannelsForPop <- function(gs, pop){
  gates <- .flowGateForPop(gs, pop)
  names(gates@parameters)
}

#' @description Get leaves from population
#' @param gs GatingSet
#' @param pop character for population
#' @param groupByParent boolean for grouping by parent
#' @param groupByGate boolean for grouping by gate
#' @examples
#' TODO
#' @export
.flowLeavesForPop <- function(gs, pop = "/", groupByParent = FALSE, groupByGate = FALSE) {
  pops <- flowWorkspace::gs_get_pop_paths(gs, order = 'bfs')
  
  leaves <- .popsGetParentLeaves(pops = pops, pop = pop)
  
  # return leaves by parent
  if (groupByParent == TRUE) {
    # group by parent
    leaveParents <- sapply(leaves, .flowPopParent, root = "root")
    uniqueParents <- sapply(
      unique(leaveParents),
      .flowNormRootPath, defaultVal = "root", USE.NAMES = FALSE)
    
    groupedLeaves <- list()
    
    # TODO is there a better way?
    for (i in uniqueParents) {
      groupedLeaves[[i]] <- c()
    }
    
    for (i in names(leaveParents)) {
      groupedLeaves[[leaveParents[[i]]]] <- c(
        groupedLeaves[[leaveParents[[i]]]], i)
    }
    
    leaves <- groupedLeaves
    
    # replace root
    names(leaves)[names(leaves) == ""] <- "root"
    
    # return leaves by gate
    if (groupByGate == TRUE) {
      for (i in names(leaves)) {
        # get gating info
        gateInfo <- lapply(leaves[[i]], function(x) {
          paste(sort(names(.flowGatingAxisForPop(gs, x))),
                collapse = " v ")
        })
        names(gateInfo) <- leaves[[i]]
        
        uniqueGates <- unique(gateInfo)
        
        groupedLeaves <- list()
        
        # TODO is there a better way?
        for (j in uniqueGates) {
          groupedLeaves[[j]] <- c()
        }
        
        for (j in names(gateInfo)) {
          groupedLeaves[[gateInfo[[j]]]] <- c(
            groupedLeaves[[gateInfo[[j]]]], j)
        }
        
        leaves[[i]] <- groupedLeaves
      }
    }
  }
  
  leaves
}

#' @description Is population root?
#' @param pop character for population
#' @examples
#' TODO
#' @export
.flowPopIsRoot <- function(pop) {
  if (all(pop %in% c("", "/", "root"))) TRUE else FALSE
}

#' @description Normalise root population
#' @param pop character for population
#' @param defaultVal character for population default
#' @param popSuffix character for population suffix
#' @param addSuffix boolean to add population suffix
#' @examples
#' TODO
#' @export
.flowNormRootPath <- function(pop, defaultVal = "/", popSuffix = "/", addSuffix = FALSE) {
  attr(pop, "suffix") <- popSuffix
  
  # is population root?
  if (.flowPopIsRoot(pop) == TRUE) {
    pop <- defaultVal
    attr(pop, "suffix") <- ""
  }
  
  # add suffix?
  if (addSuffix == TRUE)
    pop <- paste0(pop, attr(pop, "suffix"))
  
  pop
}

#' @description Direct leaves for population
#' @param gs GatingSet
#' @param pop character for population
#' @param ... passed to .flowLeavesForPop
#' @examples
#' TODO
#' @export
.flowDirectLeavesForPop <- function(gs, pop = "/", ...) {
  # get all leaves
  allLeaves <- .flowLeavesForPop(gs, pop = pop, ...)
  
  # filter for direct leaves
  if (!.flowPopIsRoot(pop)) {
    # directLeaves <- unlist(as.list(stringr::str_match(allLeaves, sprintf("^%s/[^/]+$", pop))))
    # get only leaves with length of pop + 1
    popLength <- length(unlist(stringr::str_split(pop, "/")))
    leaveLengths <- unlist(lapply(stringr::str_split(allLeaves, "/"), length))
    directLeaves <- allLeaves[leaveLengths == popLength + 1]
  } else {
    directLeaves <- if (length(allLeaves) > 0)
      unlist(as.list(stringr::str_match(allLeaves, sprintf("^/[^/]+$"))))
    else
      list()
  }
  
  directLeaves[!is.na(directLeaves)]
}

#' @description Trim path
#' @param path character for population path
#' @param pathLevels integer for path levels
#' @param fromEnd boolean whether to trim from end
#' @examples
#' TODO
#' @export
.flowTrimPath <- function(path, pathLevels = 1, fromEnd = TRUE) {
  # split path
  splitPath <- unlist(stringr::str_split(path, "/"))
  splitPath <- splitPath[splitPath != ""]
  
  # adjust levels
  if (pathLevels > length(splitPath)) {
    pathLevels <- length(splitPath) - 1
  }
  
  # return levels
  if (fromEnd == TRUE)
    trimmedPath <- splitPath[(length(splitPath) - pathLevels):length(splitPath)]
  else
    trimmedPath <- splitPath[1:pathLevels]
  
  paste(trimmedPath, collapse = "/")
}

#' @description Parent from population
#' @param path character for population path
#' @param root character for root population
#' @param getRoot boolean to return root
#' @examples
#' TODO
#' @export
.flowPopParent <- function(pop, root = "", getRoot = TRUE) {
  # replace last population with empty string
  # popParent <- stri_replace_last(pop, "", regex = "/.+$")
  if (!.flowPopIsRoot(pop)) {
    popSplit <- stringr::str_split(pop, "/")[[1]]
    popParent <- paste(popSplit[1:length(popSplit) - 1], collapse = "/")
  } else {
    popParent <- root
  }
  
  # is root parent?
  if (popParent == "") {
    if (getRoot == TRUE)
      popParent <- root
    else
      popParent <- pop
  }
  
  popParent
}

#' @description Path names for population
#' @param pop character for population path
#' @examples
#' TODO
#' @export
.flowNamesForPops <- function(pop){
  popName <- substring(stringr::str_match(pop, "/[:alnum:]+$"), first = 2) %>%
    replace_na("root")

  popName
}

#' @description All direct leaves which have been gated on a specific axis combination
#' @param gs GatingSet
#' @param pop character for population path
#' @param gateParams list of character for gating axis (2)
#' @examples
#' TODO
#' @export
.flowDirectLeavesForPopWithAxis <- function(gs, pop, gateParams){
  # get direct leaves
  directLeaves <- .flowDirectLeavesForPop(gs, pop)
  
  matchedPops <- c()
  # go through leaves and check whether they match the axis
  for (curLeave in directLeaves){
    if (.flowMatchGatingParamsForPop(gs, curLeave, gateParams)){
      matchedPops <- c(matchedPops, curLeave)
    }
  }

  matchedPops
}

#' @description replace parent pop
#' @param popPath character for population path
#' @param popToMatch character for population path to match
#' @param popToReplace character for population path to replace
#' @param checkEqual boolean to check whether match and replace are equal
#' @examples
#' TODO
#' @export
.flowPopReplaceParent <- function(popPath, popToMatch, popToReplace, checkEqual = FALSE) {
  popPath <- .flowNormRootPath(popPath)
  popToMatch <- .flowNormRootPath(popToMatch)
  
  # match pop
  popMatch <- .flowPopIsParent(popPath, popToMatch, checkEqual = checkEqual)
  
  # replace pop path
  if (popMatch == TRUE) {
    if (attr(popMatch, "equal") == TRUE) {
      popPath <- popToReplace
    } else {
      popPath <- stringr::str_replace(
        popPath,
        sprintf("^%s/", popToMatch),
        paste0(popToReplace, attr(popToMatch, "suffix"))
        )
    }
  }
  
  popPath
}

#' @description Check if population is parent
#' @param popPath character for population path
#' @param popToMatch character for population path to match
#' @param checkEqual boolean to check whether match and replace are equal
#' @examples
#' TODO
#' @export
.flowPopIsParent <- function(popPath, popToMatch, checkEqual = FALSE) {
  popPath <- .flowNormRootPath(popPath)
  popToMatch <- .flowNormRootPath(popToMatch)
  
  popsMatch <- startsWith(popPath, paste0(popToMatch, attr(popToMatch, "suffix")))
  attr(popsMatch, "equal") <- FALSE
  
  # check if they are equal
  if (checkEqual == TRUE && popsMatch == FALSE) {
    popsMatch <- popPath == popToMatch
    attr(popsMatch, "equal") <- TRUE
  }
  
  popsMatch
}

#' @description Get pop path for name and parent
#' @param popName character for population
#' @param popParent character for parent population
#' @examples
#' TODO
#' @export
.flowPopPath <- function(popName, popParent) {
  if (popParent == "root") popParent <- "/"
  if (popParent == "/") popParent <- ""
  
  paste(popParent, popName, sep = "/")
}

#' @description Rename parent name
#' @param popPath character for population
#' @param popParent character for parent population
#' @examples
#' TODO
#' @export
.flowChangeParentName <- function(popPath, parentPath) {
  a <- stringr::str_split(popPath, "/")[[1]]
  b <- stringr::str_split(parentPath, "/")[[1]]
  
  # replace in x
  for (j in seq(length(b))) {
    a[[j]] <- b[[j]]
  }
  
  # put back together
  paste(a, collapse = "/")
}

#' @description Get gating axis for pop
#' @param gs GatingSet
#' @param pop character for population
#' @examples
#' TODO
#' @export
.flowGatingAxisForPop <- function(gs, pop){
  .flowGateForPop(gs, pop)@parameters
}

#' @description Are the selected gating params used for population gating?
#' @param gs GatingSet
#' @param pop character for population
#' @param gateParams list of character for gating axis (2)
#' @examples
#' TODO
#' @export
.flowMatchGatingParamsForPop <- function(gs, pop, gateParams) {
  curGateParams <- .flowGatingAxisForPop(gs, pop)

  matchAxis <- TRUE

  # go through parameters and check
  # if one axis does not match
  for (curParam in curGateParams) {
    if (!(curParam@parameters) %in% gateParams){
      matchAxis <- FALSE
    }
  }

  matchAxis
}

#' @description colour range
#' @export
.flowColorRampBlueHeat <- function(n) {
  # Adapted from https://www.r-bloggers.com/2013/03/r-defining-your-own-color-schemes-for-heatmaps/
  red <- rgb(1,0,0)
  green <- rgb(0,1,0)
  yellow <- rgb(1,1,0)
  blue <- rgb(0,0,1)
  white <- rgb(1,1,1)
  
  colorRampPalette(c("black", "#1793ff", "#04fa00", "#ffa805", "#ff3856"))(n)
}

### RASTER
#' @description Build raster plot
#' @param DT data.table to prepare
#' @param flowX character for x column
#' @param flowY character for y column
#' @param colorMode character for color mode
#' @param color character to color
#' @param reduction_func character for reduction function
#' @param colorBy character to group data.table
#' @param xRange numeric(2) for x range
#' @param yRange numeric(2) for y range
#' @param plot_height numeric for plot height
#' @param plot_width numeric for plot width
#' @param flowColour character for coor palette
#' @examples
#' TODO
#' @export
.flowRasterBuild <- function(DT, flowX, flowY, colorMode = "dark", color = NULL,
                             reduction_func = NULL, colorBy = NULL,
                             xRange = NULL, yRange = NULL, plot_height = 256,
                             plot_width = 256, flowColour = "Spectral", ...) {
  # dummy for reduction function
  DT[, on := 1]
  
  # get max colours
  maxColours <- RColorBrewer::brewer.pal.info[flowColour,]$maxcolors
  
  # set range
  if (is.null(xRange))
    xRange <- range(DT[[flowX]])
  if (is.null(yRange))
    yRange <- range(DT[[flowY]])
  
  # adjust plot height and width from range
  # xAdj <- diff(range(DT[[flowX]]))/diff(xRange)
  # yAdj <- diff(range(DT[[flowY]]))/diff(yRange)
  
  r1 <- rasterly::rasterly(
    data = DT,
    mapping = rasterly::aes(
      x = get(flowX),
      y = get(flowY),
      on = on,
      color = if (!is.null(colorBy)) get(colorBy) else NULL
    # ), plot_width = plot_width * xAdj, plot_height = plot_height * yAdj, ...)
    ), plot_width = plot_width, plot_height = plot_height, ...)
  
  # check mode
  if (colorMode == "white") {
    r1 <- r1 %>% rasterly::rasterly_points(
      # color = if (is.null(color)) rev(RColorBrewer::brewer.pal(11, "Spectral")) else color,
      color = if (is.null(color) && is.null(colorBy)) c(
        "black", rev(RColorBrewer::brewer.pal(maxColours, flowColour))
      ) else color,
      background = "#00000000",
      glyph = "square",
      xlim = xRange,
      ylim = yRange,
      reduction_func = reduction_func
    ) %>% rasterly::rasterly_build()
  } else {
    r1 <- r1 %>% rasterly::rasterly_points(
      color = if (is.null(color) && is.null(colorBy)) rev(
        RColorBrewer::brewer.pal(maxColours, flowColour)) else color,
      background = "#22222200",
      glyph = "square",
      xlim = xRange,
      ylim = yRange,
      reduction_func = reduction_func
    ) %>% rasterly::rasterly_build()
  }
  
  # add adjustment factors
  # r1$x_adj <- xAdj
  # r1$y_adj <- yAdj
  
  r1
}

#' @description Build raster contour plot
#' @param DT data.table to prepare
#' @param flowX character for x column
#' @param flowY character for y column
#' @param color character to color
#' @param ... passed to .flowRasterBuild
#' @examples
#' TODO
#' @export
.flowRasterContour <- function(DT, flowX, flowY, colorMode = "white",
                               color = "black", ...) {
  
  # build raster for outliers
  r1 <- .flowRasterBuild(
    DT, flowX, flowY, colorMode = colorMode,
    color = color, reduction_func = "first", ...)
  
  # for contour
  r2 <- .flowRasterBuild(
    DT, flowX, flowY, colorMode = colorMode,
    # plot_height = 128 * r1$x_adj, plot_width = 128 * r1$y_adj, ...)
    plot_height = 64, plot_width = 64, ...)
  
  # get density for contours
  dens <- r2$agg$rasterlyPoints1[[1]]
  
  # normalise density
  densRange <- range(dens)
  dens <- (dens - densRange[[1]]) / (densRange[[2]] - densRange[[1]])
  
  # get contour lines
  rasterContours <- .flowContourLines(
    DT, flowX, flowY, dens = dens, pointsInContour = FALSE)
  
  # adjust XY
  rasterContours[, x := (x * diff(r1$y_range)) + r1$y_range[1]]
  rasterContours[, y := (y * diff(r1$x_range)) + r1$x_range[1]]
  
  list(
    raster = r1,
    contours = rasterContours
  )
}

#' @description Prepare raster plot
#' @param ... passed to .flowRasterBuild
#' @examples
#' TODO
#' @export
.flowRasterPrepPlotly <- function(...) {
  # build raster
  r1 <- .flowRasterBuild(...)
  
  # create image
  z <- r1$image
  dimZ <- dim(z)
  # z <- matrix(log(z + 1), nrow = dimZ[1])
  
  # from https://github.com/plotly/plotly.R/blob/c35a44ef99d2f0a06f58d5c4447576d032091523/R/add.R
  cols <- col2rgb(z, alpha = TRUE)
  dims <- c(dim(z), 4)
  z <- array(numeric(prod(dims)), dims)
  matrix_ <- function(x) {
    matrix(x, byrow = TRUE, nrow = dims[1], ncol = dims[2])
  }
  z[,,1] <- matrix_(cols["red",])
  z[,,2] <- matrix_(cols["green",])
  z[,,3] <- matrix_(cols["blue",])
  z[,,4] <- matrix_(cols["alpha",])
  
  colormodel <- "rgba"
  
  list(
    z = z,
    x0 = r1$x_range[1],
    dx = diff(r1$x_range)/dimZ[2],
    y0 = r1$y_range[2],
    dy = -diff(r1$y_range)/dimZ[1]
  )
}

#' @description List of gated raster
#' @param cciaObj CciaImage to retrieve populations from
#' @param popPaths list of character for population paths
#' @param labelSize numeric for geom_label size
#' @param xTitleSize numeric for x title size
#' @param yTitleSize numeric for y title size
#' @param xAxisSize numeric for x axis size
#' @param yAxisSize numeric for y axis size
#' @param labelBorder numeric for geom_label border size
#' @param labelAlpha numeric for geom_label alpha
#' @param labelPos list of coordinates for labels
#' @param asContours boolean to use contours
#' @param showPopColours boolean to show population colours
#' @param directLeaves boolean for direct leaves
#' @param showPopName boolean to show pop name in gate
#' @param showGatePopColours boolean to use population colours for gate
#' @param plotTitleSize numeric for title size
#' @param xRange numeric vector range for x axis
#' @param yRange numeric vector range for y axis
#' @param ... passed to .flowRasterBuild
#' @examples
#' TODO
#' @export
.flowPlotGatedRaster <- function(cciaObj, popPath = "root", labelSize = 2,
                                 labelBorder = 1, labelAlpha = 1.00,
                                 xTitleSize = 12, yTitleSize = 12,
                                 xAxisSize = 12, yAxisSize = 12,
                                 labelPos = list(), asContours = FALSE,
                                 showPopColours = FALSE, showGatePopColours = TRUE,
                                 directLeaves = FALSE, showPopName = TRUE,
                                 showAnnotation = TRUE, plotTitleSize = 14,
                                 xRange = NULL, yRange = NULL, ...) {
  # go through pops and build gating scheme
  fgs <- cciaObj$flowGatingSet()
  
  # get pop paths
  # if (is.null(popPaths))
  #   popPaths <- fgs$popPaths()
  
  # get leaves
  # TODO that should be better
  if (directLeaves == TRUE) {
    leaves <- list()
    leaves[[popPath]] <- fgs$popLeaves(popPath, directLeaves = TRUE,
                                       groupByParent = TRUE, groupByGate = TRUE)
  } else {
    leaves <- fgs$popLeaves(popPath, directLeaves = FALSE,
                            groupByParent = TRUE, groupByGate = TRUE)
  }
  
  p1s <- list()
  for (xParent in names(leaves)) {
    for (xGateParams in names(leaves[[xParent]])) {
      local({
        pops <- leaves[[xParent]][[xGateParams]]
        
        # go through pops and get gates
        popGates <- lapply(pops, fgs$popGate)
        names(popGates) <- pops
        
        # get channels
        popGateChannels <- lapply(popGates, function(x) names(x@parameters))
        
        xPops <- c("root", xParent)
        
        if (showPopColours == TRUE) {
          # get all direct leaves of the leaves
          directLeaves <- unlist(lapply(pops, fgs$popDirectLeaves))
          
          if (.flowPopIsRoot(xParent))
            xPops <- c(xParent, directLeaves)
          else
            xPops <- c("root", xParent, directLeaves)
        }
        
        # get pops
        popDT <- cciaObj$popDT(
          "flow", pops = xPops, popCols = unique(unlist(popGateChannels)),
          completeDT = FALSE, uniqueLabels = TRUE)
        
        # go through gate combinations
        for (gateChannels in unique(popGateChannels)) {
          xLabel <- gateChannels[[1]]
          yLabel <- gateChannels[[2]]
          
          # get range
          if (is.null(xRange))
            xRange <- range(popDT[, ..xLabel])
          if (is.null(yRange))
            yRange <- range(popDT[, ..yLabel])
          
          # get boundaries
          gateDTs <- list()
          
          for (j in names(popGates)) {
            xGate <- popGates[[j]]
            
            # rectangle gate?
            if (attr(xGate, "class") == "rectangleGate") {
              # build path
              gateDTs[[j]] <- as.data.table(list(
                x = c(xGate@min[1], xGate@min[1], xGate@max[1], xGate@max[1]),
                y = c(xGate@min[2], xGate@max[2], xGate@max[2], xGate@min[2])
              ))
              setnames(gateDTs[[j]], "x", xLabel)
              setnames(gateDTs[[j]], "y", yLabel)
            } else {
              # close path
              gateDTs[[j]] <- as.data.table(xGate@boundaries)
              gateDTs[[j]] <- rbind(gateDTs[[j]], gateDTs[[j]][1])
            }
          }
          
          # bind together
          gateDT <- rbindlist(gateDTs, idcol = "pop")
          
          # get gate range
          xRangeGate <- range(gateDT[, ..xLabel])
          yRangeGate <- range(gateDT[, ..yLabel])
          
          # combine with data range
          xRange <- c(floor_dec(min(xRange[[1]], xRangeGate[[1]]), level = 4),
                      ceiling_dec(max(xRange[[2]], xRangeGate[[2]]), level = 4))
          yRange <- c(floor_dec(min(yRange[[1]], yRangeGate[[1]]), level = 4),
                      ceiling_dec(max(yRange[[2]], yRangeGate[[2]]), level = 4))
          
          # add label
          # add label
          # nameDT <- as.data.frame(rbind(gateDT %>% colMeans()))
          nameDTs <- list()
          
          for (j in names(popGates)) {
            nameDTs[[j]] <- as.data.frame(list(
              x = mean(gateDT[pop == j, ][[xLabel]]),
              # x = min(gateDT[pop == j, ][[xLabel]]),
              # x = max(gateDT[pop == j, ][[xLabel]]),
              # x = quantile(gateDT[pop == j, ][[xLabel]], 0.75),
              y = max(gateDT[pop == j, ][[yLabel]])
              # y = mean(gateDT[pop == j, ][[yLabel]])
            ))
            
            if (j %in% names(labelPos)) {
              if ("x" %in% names(labelPos[[j]]))
                nameDTs[[j]]$x <- labelPos[[j]]$x
              if ("y" %in% names(labelPos[[j]]))
                nameDTs[[j]]$y <- labelPos[[j]]$y
              if ("yFun" %in% names(labelPos[[j]]))
                nameDTs[[j]]$y <- labelPos[[j]]$yFun(gateDT[pop == j, ][[yLabel]])
            }
            
            colnames(nameDTs[[j]]) <- c(xLabel, yLabel)
            
            # add pop name
            if (showPopName == TRUE)
              nameDTs[[j]]$label <- .flowTrimPath(j, pathLevels = 0)
            else
              nameDTs[[j]]$label <- ""
            
            # add percentage
            if (showAnnotation == TRUE) {
              popStats <- fgs$getPopStats(j, type = "percent")
              
              nameDTs[[j]]$label <- paste(
                nameDTs[[j]]$label, paste0(
                  sprintf("%0.2f", popStats$percent * 100), "%"))
            }
          }
          
          # bind together
          nameDT <- rbindlist(nameDTs, idcol = "pop")
          
          # prepare colours
          if (showPopColours == TRUE) {
            # show population colours
            popColours <- as.list(sapply(
              xPops,
              function(x) cciaObj$popAttr("flow", "colour", popPath = x)[[1]]
            ))
            
            # adjust root colour
            popColours$root <- "black"
            
            popExclude <- ""
            if (!.flowPopIsRoot(xParent)) {
              popExclude <- "root"
              popColours <- popColours[names(popColours) != "root"]
            }
          }
          
          # prepare plot
          p1 <- ggplot2::ggplot() +
            ggplot2::theme_classic() +
            # xlim(xRange) + ylim(yRange) +
            # https://stackoverflow.com/a/21307010
            ggplot2::xlim(range(pretty(xRange))) +
            ggplot2::ylim(range(pretty(yRange))) +
            ggplot2::xlab(xLabel) +
            ggplot2::ylab(yLabel)
          
          # build raster
          if (asContours == TRUE) {
            if (showPopColours == TRUE) {
              # get contours for pops
              rasterContours <- list()
              
              for (y in xPops[!xPops %in% popExclude]) {
                rasterContours[[y]] <- .flowRasterContour(
                  popDT[pop == y], xLabel, yLabel, color = popColours[[y]],
                  xRange = xRange, yRange = yRange)
              }
              
              # add rasters
              for (j in names(rasterContours)) {
                y <- rasterContours[[j]]
                
                p1 <- p1 + ggplot2::annotation_raster(
                  y$raster$image,
                  xmin = y$raster$x_range[1],
                  xmax = y$raster$x_range[2],
                  ymin = y$raster$y_range[1],
                  ymax = y$raster$y_range[2]
                ) +
                  ggplot2::geom_polygon(
                    data = y$contours,
                    aes(x = y, y = x, group = as.factor(seq)),
                    size = 0.2, color = popColours[[j]], fill = "white"
                  )
              }
            } else {
              r1 <- .flowRasterContour(
                popDT[pop == xParent], xLabel, yLabel,
                xRange = xRange, yRange = yRange, ...)
              
              p1 <- p1 + 
                ggplot2::annotation_raster(
                  r1$raster$image,
                  xmin = r1$raster$x_range[1],
                  xmax = r1$raster$x_range[2],
                  ymin = r1$raster$y_range[1],
                  ymax = r1$raster$y_range[2]) +
                ggplot2::geom_polygon(
                  data = r1$contours,
                  aes(x = y, y = x, group = as.factor(seq)),
                  size = 0.2, color = "black", fill = "white") 
            }
          } else {
            if (showPopColours == TRUE) {
              # build raster
              r1 <- .flowRasterBuild(
                popDT[pop != popExclude], xLabel, yLabel,
                colorMode = "white", layout = "cover",
                colorBy = "pop", color = popColours,
                xRange = xRange, yRange = yRange, ...)
            } else {
              r1 <- .flowRasterBuild(
                popDT[pop == xParent], xLabel, yLabel,
                colorMode = "white", layout = "cover",
                xRange = xRange, yRange = yRange, ...)
            }
            
            p1 <- p1 +
              annotation_raster(
                r1$image,
                xmin = r1$x_range[1], xmax = r1$x_range[2],
                ymin = r1$y_range[1], ymax = r1$y_range[2]) 
          }
          
          # get population colors
          if (showGatePopColours == TRUE) {
            popColors <- sapply(
              pops, function(x) cciaObj$popAttr("flow", "colour", popPath = x)[[1]])
          } else {
            popColors <- rep("white", length(pops))
          }
          
          # add gates
          p1 <- p1 + ggplot2::geom_polygon(
            data = gateDT,
            aes(
              x = get(xLabel),
              y = get(yLabel),
              group = pop
            ), size = 0.5, color = "black",
            # fill = "#23aeff", alpha = 0.2) +
            alpha = 0.0) +
            # ggplot2::geom_label(
            ggrepel::geom_label_repel(
              data = nameDT,
              aes(
                label = label,
                x = get(xLabel),
                y = get(yLabel),
                group = pop,
                fill = pop
                ),
              color = "black",
              fontface = "bold",
              # size = labelSize, color = cciaObj$popAttr("flow", "colour", popPath = x)[[1]]
              size = labelSize, label.size = labelBorder, alpha = labelAlpha
            ) +
            # scale_color_manual(values = popColors) +
            scale_fill_manual(values = popColors) +
            ggtitle(xParent) +
            theme(
              legend.position = "none",
              # plot.title = element_text(size = 8)
              plot.title = element_text(size = plotTitleSize),
              axis.title.x = element_text(size = xTitleSize),
              axis.title.y = element_text(size = yTitleSize),
              axis.text.x = element_text(size = xAxisSize),
              axis.text.y = element_text(size = yAxisSize),
            )
          
          # make fixed if coords are shown
          if (all(!is.na(str_match(c(xLabel, yLabel), "centroid"))))
            p1 <- p1 + coord_fixed()
          
          p1s[[paste(xParent, xGateParams, sep = ":")]] <<- p1
        }
      })
    }
  }
  
  p1s
}
