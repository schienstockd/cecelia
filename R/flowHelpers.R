#' @description Correct channel names
#' @param channelNames list of character for channel names
#' @examples
#' TODO
.flowCorrectChannelNames <- function(channelNames) {
  channelNames <- gsub(" |\\(|\\)|\\-", ".", channelNames)
  channelNames <- gsub("\\.+", ".", channelNames)
  channelNames <- gsub("\\.$", "", channelNames)
  
  channelNames
}

#' @description Generate colours for flow plots
#' @param x list of numeric for 'X'-values
#' @param y list of numeric for 'Y'-values
#' @examples
#' TODO
.flowColours <- function(x, y, nbin = 128) {
  retVal <- NULL
  
  if (all(length(x) > 0, length(y) > 0)) {
    retVal <- densCols(
      x, y, nbin = nbin,
      colramp = flowViz::flowViz.par.get("argcolramp"))
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
  # confidenceLines <- contourLines(dens, levels = contourLevels)
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
.flowCreateGatingSet <- function(ffs, channelNames, ffNames = NULL,
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

#' @description Fortify gating set
#' @importFrom ggplot2 fortify
#' @import ggcyto
#' 
#' @param gs GatingSet
#' @param subset character of population subset
#' @examples
#' TODO
.flowFortifyGs <- function(gs, subset = "root", cols) {
  retVal <- NULL
  
  tryCatch(
    expr = {
      # fortify gating set
      attr(gs, "subset") <- subset
      if (!is.null(cols))
        attr(gs, "dims") <- data.table(name = cols)
      retVal <- fortify(gs)
    },
    error = function(e){ 
      retVal <<- NULL
    },
    warning = function(w){
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

#' @description Compensate with linear model
#' @param df data.frame to compensate
#' @param channelNames list of character for channel names
#' @param refAxis character for reference axis
#' @param polyDegree integer for plynomial degress
#' @param suffix character for column suffix
#' @param replaceValues boolean to replace previous values
#' @param flowNames boolean to use "flow-names"
#' @examples
#' TODO
.flowCompensatePoly <- function(df, channelNames, refAxis,
                               polyDegree = 4, suffix = ".corr",
                               replaceValues = FALSE, flowNames = TRUE) {
  # correct channel names before selecting
  if (flowNames == TRUE) {
    # correct channel names
    channelNames <- .flowCorrectChannelNames(channelNames)
  }
  
  # correct all channels
  for (x in channelNames) {
    # fit model
    # https://stackoverflow.com/a/3822706/13766165
    polyEstimate <- lm(get(x) ~ poly(get(refAxis), polyDegree, raw = FALSE),
                       data = df)
    
    # predict values
    polyPredict <- predict(
      polyEstimate,
      newdata = data.frame(x = df[[refAxis]]) %>%
        rename_with(.cols = 1, ~refAxis))
    
    # correct values to mean
    meanIntensity <- mean(df[[x]])
    polyCorrected <- polyPredict / meanIntensity
    
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
.flowGateForPop <- function(gs, pop) {
  # there will only be one gate returned
  gs_pop_get_gate(gs, pop)[[1]]
}

#' @description Get stats for pop
#' @param gs GatingSet
#' @param pop character for population
#' @param ... passed to gs_pop_get_stats
#' @examples
#' TODO
.flowStatsForPop <- function(gs, pop, ...) {
  gs_pop_get_stats(gs, pop, ...)
}

#' @description Get channels for pop
#' @param gs GatingSet
#' @param pop character for population
#' @examples
#' TODO
.flowChannelsForPop <- function(gs, pop){
  gates <- .flowGateForPop(gs, pop)
  names(gates@parameters)
}

#' @description Get leaves from population
#' @param gs GatingSet
#' @param pop character for population
#' @examples
#' TODO
.flowLeavesForPop <- function(gs, pop = "/") {
  pops <- gs_get_pop_paths(gs, order = 'bfs')
  
  # return parent leaves
  .popsGetParentLeaves(pops = pops, pop = pop)
}

#' @description Is population root?
#' @param pop character for population
#' @examples
#' TODO
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
#' @examples
#' TODO
.flowDirectLeavesForPop <- function(gs, pop = "/") {
  # get all leaves
  allLeaves <- .flowLeavesForPop(gs, pop = pop)
  
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
#' @examples
#' TODO
.flowTrimPath <- function(path, pathLevels = 1) {
  # split path
  splitPath <- unlist(stringr::str_split(path, "/"))
  splitPath <- splitPath[splitPath != ""]
  
  # adjust levels
  if (pathLevels > length(splitPath)) {
    pathLevels <- length(splitPath) - 1
  }
  
  # return levels
  trimmedPath <- splitPath[(length(splitPath) - pathLevels):length(splitPath)]
  
  paste(trimmedPath, collapse = "/")
}

#' @description Parent from population
#' @param path character for population path
#' @param root character for root population
#' @examples
#' TODO
.flowPopParent <- function(pop, root = "") {
  # replace last population with empty string
  # popParent <- stri_replace_last(pop, "", regex = "/.+$")
  if (!.flowPopIsRoot(pop)) {
    popSplit <- stringr::str_split(pop, "/")[[1]]
    popParent <- paste(popSplit[1:length(popSplit) - 1], collapse = "/")
  } else {
    popParent <- root
  }
  
  # is root parent?
  if (popParent == "")
    popParent <- root
  
  popParent
}

#' @description Path names for population
#' @param pop character for population path
#' @examples
#' TODO
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
.flowGatingAxisForPop <- function(gs, pop){
  .flowGateForPop(gs, pop)@parameters
}

#' @description Are the selected gating params used for population gating?
#' @param gs GatingSet
#' @param pop character for population
#' @param gateParams list of character for gating axis (2)
#' @examples
#' TODO
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
#' @examples
#' TODO
.flowRasterBuild <- function(DT, flowX, flowY, colorMode = "dark", color = NULL,
                             reduction_func = NULL, colorBy = NULL, ...) {
  # dummy for reduction function
  DT[, on := 1]
  
  r1 <- rasterly::rasterly(
    data = DT,
    mapping = rasterly::aes(
      x = get(flowX),
      y = get(flowY),
      on = on,
      color = if (!is.null(colorBy)) get(colorBy) else NULL
    ), ...)
  
  # check mode
  if (colorMode == "white") {
    r1 <- r1 %>% rasterly::rasterly_points(
      # color = if (is.null(color)) rev(RColorBrewer::brewer.pal(11, "Spectral")) else color,
      color = if (is.null(color) && is.null(colorBy)) c(
        "black", rev(RColorBrewer::brewer.pal(11, "Spectral"))
      ) else color,
      background = "#00000000",
      glyph = "square",
      xlim = range(DT[[flowX]]),
      ylim = range(DT[[flowY]]),
      reduction_func = reduction_func
    ) %>% rasterly::rasterly_build()
  } else {
    r1 %>% rasterly::rasterly_points(
      color = if (is.null(color) && is.null(colorBy)) rev(
        RColorBrewer::brewer.pal(11, "Spectral")) else color,
      background = "#22222200",
      glyph = "square",
      xlim = range(DT[[flowX]]),
      ylim = range(DT[[flowY]]),
      reduction_func = reduction_func
    ) %>% rasterly::rasterly_build()
  }
}

#' @description Build raster contour plot
#' @param DT data.table to prepare
#' @param flowX character for x column
#' @param flowY character for y column
#' @param color character to color
#' @examples
#' TODO
.flowRasterContour <- function(DT, flowX, flowY, colorMode = "white", color = NULL) {
  # build raster for outliers
  r1 <- .flowRasterBuild(
    DT, flowX, flowY, colorMode = colorMode,
    color = color, reduction_func = "first")
  
  # for contour
  r2 <- .flowRasterBuild(
    DT, flowX, flowY, colorMode = colorMode, 
    plot_height = 50, plot_width = 50)
  
  # get density for contours
  dens <- r2$agg$rasterlyPoints1[[1]]
  
  # normalise density
  densRange <- range(dens)
  dens <- (dens - densRange[[1]]) / (densRange[[2]] - densRange[[1]])
  
  # get contour lines
  rasterContours <- .flowContourLines(
    popDT, xLabel, yLabel, dens = dens, pointsInContour = FALSE)
  
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
