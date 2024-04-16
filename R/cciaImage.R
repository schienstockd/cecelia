#' Reactive image object
#' 
#' @name CciaImage
#' @description Reactive image object
#' @import data.table
#'
#' @examples
#' TODO
#' @export
CciaImage <- R6::R6Class(
  "CciaImage",
  inherit = ReactivePersistentObject,
  
  ## private
  private = list(
    # mapping
    anndataUtils = NULL,
    anndataUsedChannels = NULL,
    
    # ome XML
    handleOmeXML = NULL,
    handleOmeXMLPixels = NULL,
    handleTimelapseInfo = NULL,
    
    # labels
    labelPropsUtils = NULL,
    labelTracks = NULL,
    labelTracksInfo = NULL,
    regionPropsUtils = NULL,
    neighourPropsUtils = NULL,
    
    # populations
    filteredPopDT = NULL,
    
    # live
    handleLivePopUtils = NULL,
    
    # flow
    handleFlowGatingSet = NULL,
    
    # classifications
    handleClsfPopUtils = NULL,
    
    # branching
    handleBranchPopUtils = NULL,
    
    # labels
    handleLabelsPopUtils = NULL,
    
    # spatial
    handleSpatialDT = NULL,
    handleSpatialGraph = NULL,
    
    #' @description Complete population DT
    #' @param popUtils PopulationUtils
    #' @param popUtils PopulationUtils
    #' @param popCols list of character
    #' @param uniqueLabels boolean to force unique labels
    #' @param includeX boolean to include 'X' from adata
    #' @param replaceX boolean to replace 'X' with 'X' from adata
    #' @param includeObs boolean to include 'obs' from adata
    #' @param valueNames character to load label props
    completePopDT = function(popUtils, popDT, popCols = NULL,
                             uniqueLabels = TRUE, includeX = FALSE,
                             replaceX = FALSE, includeObs = TRUE,
                             valueNames = c()) {
      # add columns from label properties that are
      # not in the popUtils DT
      if (popUtils$isLabelPropsStore() == FALSE) {
        if (!length(valueNames) > 0)
          valueNames <- popUtils$getValueNames()
        
        # get value names for populations
        for (i in valueNames) {
          labels <- self$labelProps(valueName = i)
          
          if (!is.null(labels)) {
            # focus on selected columns
            if (!is.null(popCols)) {
              labels$view_cols(popCols)
            }
            
            # filter on labels to reduce reading
            if ("value_name" %in% colnames(popDT)) {
              # TODO not sure how to do this for this case
            } else {
              labels$filter_rows(popDT$label)
            }
            
            # set include 'X'
            if (replaceX == TRUE)
              includeX <- TRUE
            
            # only complete obs
            # TODO is that OK? - not always
            labelDT <- as.data.table(labels$as_df(
              include_x = includeX, add_obs = includeObs))
            # labelDT <- as.data.table(labels$values_obs())
            # labels$close()
            
            # prepare column names
            # exclude channel names
            labelColumns <- colnames(labelDT)
            if (replaceX == FALSE) {
              labelColumns <- labelColumns[!labelColumns %in% self$imChannelNames(
                correctChannelNames = TRUE, includeTypes = TRUE)]
              iColumns <- labelColumns
            } else {
              jColumns <- labelColumns[labelColumns %in% colnames(popDT)]
              jColumns <- jColumns[jColumns != "label"]
              iColumns <- labelColumns
              iColumns[iColumns %in% jColumns] <- paste0("i.", jColumns)
            }
            
            # merge to population DT
            if ("label" %in% colnames(popDT)) {
              if ("value_name" %in% colnames(popDT)) {
                popDT[labelDT, on = .(value_name == i, label),
                      (labelColumns) := mget(iColumns)]
              } else {
                popDT[labelDT, on = .(label),
                      (labelColumns) := mget(iColumns)]
              }
            }
          }
        }
      }
      
      # remove duplicates
      if (uniqueLabels == TRUE) {
        # TODO it would be better to do this in place
        # and let the population utils do that
        
        # define sort columns
        sortCols <- c("pop", "label", "track_id")
        sortCols <- sortCols[sortCols %in% colnames(popDT)]
        sortOrder = c(-1, rep(1, length(sortCols) - 1))
        
        if ("value_name" %in% colnames(popDT)) {
          sortCols <- c("value_name", sortCols)
          sortOrder <- c(1, sortOrder)
        }
        
        # order by pop and label
        if (nrow(popDT) > 0)
          setorderv(popDT, sortCols, sortOrder)
        
        # remove duplicates for pop and label
        popDT <- unique(popDT, by = sortCols[sortCols != "pop"])
      }
      
      popDT
    },
    
    # setters
    setOmeXML = function(x, invalidate = TRUE, reset = FALSE) {
      private$handleOmeXML <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setOmeXMLPixels = function(x, invalidate = TRUE, reset = FALSE) {
      private$handleOmeXMLPixels <- x
      private$invalidate(invalidate = invalidate)
    },
    
    # getters
    getOmeXML = function() {
      private$handleOmeXML 
    },
    
    getOmeXMLPixels = function() {
      private$handleOmeXMLPixels 
    }
  ),
  
  ### public
  public = list(
    #' @description init
    #' @param stateFile character with file path to state file
    initialize = function(stateFile, ...) {
      super$initialize(stateFile = stateFile, ...)
      
      # create directories for processing
      for (x in cciaConf()$dirs$tasks) {
        dir.create(file.path(dirname(stateFile), x),
                   showWarnings = FALSE)
      }
    },
    
    #' @description Summary of image
    #' @param fields list of character for fields to include
    summary = function(fields = NULL) {
      objSummary <- super$summary(fields)
      
      # append channel names
      if ("ChannelNames" %in% fields) {
        objSummary <- append(
          objSummary,
          self$imChannelNames())
      }
      
      # append time
      if ("Time" %in% fields) {
        timeSec <- self$omeXMLPixels()$SizeT * self$omeXMLTimelapseInfo()$interval * 60
        objSummary$Time <- suppressWarnings(paste(
          lubridate::hour(timeSec),
          lubridate::minute(timeSec),
          round(lubridate::second(timeSec), digits = 0),
          sep = ":"))
      }
      
      objSummary
    },
    
    #' @description Edit channel names
    #' @param objChannelNum integer for channel number
    #' @param objChannelName character for channel name
    #' @param invalidate boolean to invalidate object
    editChannelName = function(objChannelNum, objChannelName, invalidate = TRUE, ...) {
      # edit attribute if exist
      objChannels <- self$imChannelNames()
      
      if (objChannelNum %in% names(objChannels)) {
        objChannels[[objChannelNum]] <- objChannelName
      }
      
      # push back
      self$setImChannelNames(objChannels, invalidate = invalidate, ...)
    },
    
    #' @description OME xml
    #' @param reset boolean to reset values
    omeXML = function(reset = FALSE) {
      # is content already set?
      if (is.null(private$getOmeXML()) || reset == TRUE) {
        omeXMLFilepath <- NULL
        
        # read metadata from image / file
        if (endsWith(self$imFilepath(), ".zarr")) {
          omeXMLFilepath <- file.path(self$imFilepath(), "METADATA.ome.xml")
          
          if (file.exists(omeXMLFilepath) == FALSE) {
            omeXMLFilepath <- file.path(
              self$imFilepath(), "OME", "METADATA.ome.xml"
            )
          }
          
          omeXML <- xml2::read_xml(omeXMLFilepath)
        } else {
          omeXMLFilepath <- self$imFilepath()
          
          # TODO sometimes there is an initial Null-pointer exception
          # I am not sure why that is happening
          # so, open the file again and read
          # NOTE: (!) This does not work in a forked process
          omeXMLContent <- NULL
          while(is.null(omeXMLContent)) {
            tryCatch(
              expr = {
                # get XML
                omeXMLContent <- RBioFormats::read.omexml(omeXMLFilepath)
              },
              error = function(e) { 
                message(">> Get XML content again")
              },
              warning = function(w) {
              },
              finally = {
              }
            )
          }
          
          # ensure it is utf-8
          # otherwise "µm" will throw an error during read_xml
          # Unescaped '<' not allowed in attributes values [38]
          Encoding(omeXMLContent) <- "UTF-8"
          
          suppressWarnings({
            omeXML <- xml2::read_xml(omeXMLContent)
          })
        }
        
        # https://stackoverflow.com/q/45866491/13766165
        xml2::xml_ns_strip(omeXML)
        
        # set
        private$setOmeXML(omeXML)
      }
      
      private$getOmeXML()
    },
    
    #' @description Modify xml path if needed for OME
    #' @param x character of path
    omeXMLPath = function(x) {
      if (startsWith(xml2::xml_path(self$omeXML()), "/ome:") == TRUE) {
        return(stringr::str_replace_all(x, "//", "//ome:"))
      } else {
        return(x)
      }
    },
    
    #' @description pixel information
    #' @param reset boolean to reset value
    omeXMLPixels = function(reset = FALSE, omeXML = NULL) {
      # is content already set?
      if (is.null(private$getOmeXMLPixels()) || reset == TRUE) {
        pixelInfo <- as.list(unlist(xml2::xml_attrs(
          xml2::xml_find_all(self$omeXML(reset = reset), self$omeXMLPath("//Image//Pixels"))
        )))
        
        # convert to numeric
        pixelContent <- suppressWarnings({
          lapply(pixelInfo, function(x) {
            if (!is.na(as.numeric(x)))
              as.numeric(x)
            else
              x
          })
        })
        
        # TODO more elegant
        # add physical units and sizes if not given
        unitsToAdd <- c("PhysicalSizeXUnit", "PhysicalSizeYUnit")
        sizesToAdd <- c("PhysicalSizeX", "PhysicalSizeY")
        for (x in unitsToAdd) 
          if (!x %in% names(pixelContent))
            pixelContent[[x]] <- "um"
        for (x in sizesToAdd) 
          if (!x %in% names(pixelContent))
            pixelContent[[x]] <- 1
        
        # set
        private$setOmeXMLPixels(pixelContent)
      }
      
      private$getOmeXMLPixels()
    },
    
    #' @description Timelapse information
    omeXMLTimelapseInfo = function() {
      tInfo <- private$handleTimelapseInfo
      
      if (is.null(tInfo)) {
        tInfo <- list(
          interval = 1
        )
        
        basenameOriFilepath <- basename(self$oriFilepath())
        
        # return information for timelapse
        # this will probably be dependent on the microscope
        if (endsWith(basenameOriFilepath, ".oir")) {
          # TODO is there a better way?
          # get values in annotations
          suppressWarnings({
            valueChildren <- xml2::xml_children(xml2::xml_find_all(
              self$omeXML(), self$omeXMLPath("//StructuredAnnotations//Value")))
            
            # get timelapse axis information
            timelapseAxisNodes <- valueChildren[!is.na(
              stringr::str_match(as.character(valueChildren), "TIMELAPSE"))]
            
            axisNum <- as.numeric(
              stringr::str_extract(
                as.character(xml2::xml_contents(xml2::xml_contents(timelapseAxisNodes))), "(?<=axis #)[0-9]$")
            )
            axisNum <- axisNum[!is.na(axisNum)]
            
            # get interval
            axisValue <- valueChildren[!is.na(stringr::str_match(as.character(valueChildren), sprintf("step #%s", axisNum)))]
            
            # put information into list
            tInfo <- list(
              interval = as.numeric(stringr::str_extract(
                as.character(xml2::xml_contents(xml2::xml_contents(axisValue)[2])),
                "[0-9]+\\.[0-9]+")) / 60
            )
          })
        } else if (stringr::str_ends(basenameOriFilepath, "\\.lsm|tif")) {
          # get interval from pixel information
          tInfo <- list(
            interval = self$omeXMLPixels()$TimeIncrement / 60
          )
        } else if (endsWith(basenameOriFilepath, ".ims")) {
          # TODO is there a better way?
          # get values in annotations
          suppressWarnings({
            valueChildren <- xml2::xml_children(xml2::xml_find_all(
              self$omeXML(), self$omeXMLPath("//StructuredAnnotations//Value")))
            
            # get time step information
            timeStepNode <- valueChildren[!is.na(stringr::str_match(as.character(valueChildren), "Time_Step")[,1])]
            
            if (length(timeStepNode) == 0) {
              timeStepNode <- valueChildren[!is.na(stringr::str_match(as.character(valueChildren), "Time_Interval_\\[s\\]")[,1])]
            }
            
            # put information into list
            tInfo <- list(
              interval = as.numeric(stringr::str_extract(as.character(xml2::xml_contents(xml2::xml_contents(timeStepNode)[2])), "[0-9]+\\.[0-9]+")) / 60
            )
          })
        }
      }
      
      # if the interval is '0', take the information
      # from the metadata
      if (is.na(tInfo$interval) || length(tInfo$interval) == 0 || tInfo$interval == 0) {
        if (length(self$getCciaAttr("TimelapseInterval")) > 0)
          tInfo$interval <- as.double(self$getCciaAttr("TimelapseInterval")) / 60
        else
          tInfo$interval <- 1
      } 
      
      tInfo
    },
    
    #' @description Pixel resolution
    omeXMLPixelRes = function() {
      pixelInfo <- self$omeXMLPixels()
      
      # get scaling factor
      scaleFactorX <- 1
      scaleFactorY <- 1
      scaleFactorZ <- 1
      
      # TODO is there a better way to do this .. ?
      if (pixelInfo$PhysicalSizeXUnit == "mm")
        scaleFactorX <- 1000
      if (pixelInfo$PhysicalSizeYUnit == "mm")
        scaleFactorY <- 1000
      if ("PhysicalSizeZUnit" %in% names(pixelInfo) && pixelInfo$PhysicalSizeZUnit == "mm")
        scaleFactorZ <- 1000
      
      # return list for sizes in um
      list(
        # xy = pixelInfo$PhysicalSizeX * scaleFactorX,
        z = if ("PhysicalSizeZ" %in% names(pixelInfo))
          pixelInfo$PhysicalSizeZ * scaleFactorZ
        else
          NA,
        y = pixelInfo$PhysicalSizeY * scaleFactorY,
        x = pixelInfo$PhysicalSizeY * scaleFactorX
      )
    },
    
    #' @description Reset meta information from file
    resetMetaFromFile = function() {
      if (!is.null(self$imFilepath()) &&
          !purrr::is_empty(self$imFilepath()) &&
          file.exists(self$imFilepath())) {
        # get pixel information
        pixelInfo <- self$omeXMLPixels()
        
        # set image channels
        # extract names from file if present
        # else assign 1 2 3 .. as names
        
        # get channel size
        channelNames <- xfun::numbers_to_words(
          seq(as.integer(pixelInfo$SizeC)))
        
        # # get global metadata
        # if ("globalMetadata" %in% names(imMeta)) {
        #   imGlobalMetadata <- imMeta$globalMetadata
        # } else {
        #   imGlobalMetadata <- imMeta[[1]]$globalMetadata
        # }
        # 
        # # For OPAL
        # if (any(startsWith(names(imGlobalMetadata), "Name #"))) {
        #   channelNames <- imGlobalMetadata[startsWith(names(imGlobalMetadata), "Name #")]
        #   names(channelNames) <- paste0("Chn", stringr::str_match(names(channelNames), "[0-9]+"))
        #   channelNames <- channelNames[sort(names(channelNames))]
        # }
        
        imChannelNames <- channelNames
        
        # push back to object
        self$setImChannelNames(imChannelNames)
      }
    },
    
    #' @description Tracks info
    #' @param trackStatsNames list of character to define which stats to return
    #' @param parentPop character to define parent population
    #' @param pop character to define population
    #' @param addPops character to define populations to add
    #' @param forceReload boolean to force reload of tracks
    #' @param flushCache boolean to flush cache
    #' @param replaceNA boolean to replace NA
    #' @param quantiles list of N=2 to define quantiles
    #' @param colsToNormalise list of character to define columns to normalise
    #' @param normPercentile numeric to define normalisation percentile 0-1
    #' @param addPop boolean to add population to DT
    tracksInfo = function(trackStatsNames, parentPop, pop = NULL,
                          addPops = NULL, forceReload = FALSE, flushCache = FALSE,
                          replaceNA = TRUE, quantiles = c(0.95, 0.05),
                          colsToNormalise = c(), normPercentile = 0.998, addPop = FALSE) {
      versionedVarName <- if (!is.null(pop)) pop else parentPop
      if (!is.null(trackStatsNames)) {
        versionedVarName <- paste(
          versionedVarName,
          paste(trackStatsNames, collapse = "&"),
          sep = ":"
        )
      }
      
      # create md5 hash
      versionedVarName <- digest::digest(versionedVarName)
      
      # was this requested before?
      tracksInfo <- .getVersionedVar(
        private$labelTracksInfo, valueName = versionedVarName)
      
      if (is.null(tracksInfo) || forceReload == TRUE) {
        pops <- if (!is.null(addPops) && length(addPops) > 0)
          c(parentPop, addPops)
        else
          parentPop
        
        # define popCols
        popCols <- c("centroids", "track_id", trackStatsNames)
        
        # get population DT
        popDT <- self$popDT("live", pops = pops, includeFiltered = TRUE,
                            popCols = popCols, forceReload = forceReload,
                            flushCache = flushCache)
        
        # normalise DT
        if (length(colsToNormalise) > 0) {
          popDT <- .normaliseDT(
            popDT,
            colsToNormalise = colsToNormalise,
            normPercentile = normPercentile,
            # normalise each segmentation
            batchGroup = "value_name"
          )
        }
        
        # drop NA
        # popDT <- popDT[complete.cases(popDT),]
        
        if (!is.null(popDT) && nrow(popDT) > 0 && "track_id" %in% colnames(popDT)) {
          # get all track statistics?
          # get types for track stats
          trackStats <- sapply(trackStatsNames, .cciaStatsType)
          names(trackStats) <- trackStatsNames
          
          # get columns for stats
          if (addPop == TRUE)
            tracksInfo <- popDT[, .(num_cells = .N), by = .(pop, track_id)]
          else
            tracksInfo <- popDT[, .(num_cells = .N), by = .(track_id)]
          
          obsCols <- c()
          
          # create stats for tracks
          for (i in names(trackStats)) {
            x <- trackStats[[i]]
            
            if (i %in% names(popDT) && nrow(popDT) > 0) {
              # merge names for population frequency
              if (x %in% c("logical", "categorical")) {
                # create name for stats
                statsName <- sprintf("%s.freq", i)
                
                # convert to logical
                if (x == "logical") {
                  popDT[, c(i) := as.logical(get(i))]
                }
                
                # group
                DT.grouped <- popDT[, .(n = .N), by = .(track_id, get(i))]
                
                # average
                DT.grouped[, freq := n/sum(n), by = .(track_id)]
                
                # drop na
                DT.grouped <- DT.grouped[complete.cases(DT.grouped),]
                
                # prepare new names
                # obsColNames <- unique(DT.grouped[, get])
                obsColNames <- as.character(unique(DT.grouped$get))
                names(obsColNames) <- obsColNames
                
                # replace NA
                # names(obsColNames)[is.na(names(obsColNames))] <- "NA"
                
                obsColNames <- sapply(obsColNames, function(x) sprintf(
                  "%s.%s", i, x
                ))
                
                # cast wider
                # https://stackoverflow.com/a/65048192/13766165
                DT.grouped.freq <- dcast(DT.grouped %>% drop_na(), track_id~get, value.var = "freq")
                DT.grouped.n <- dcast(DT.grouped %>% drop_na(), track_id~get, value.var = "n")
                
                # rename columns
                for (j in names(obsColNames)) {
                  setnames(DT.grouped.freq, j, obsColNames[[j]])
                  setnames(DT.grouped.n, j, paste0(obsColNames[[j]], ".n"))
                }
                
                # merge frequency and n
                DT.grouped <- merge(DT.grouped.freq,
                                    DT.grouped.n)
                
                # add n for column names
                obsColNames <- c(
                  obsColNames,
                  sapply(obsColNames, function(x) sprintf(
                    "%s.n", x
                  ))
                )
              } else if (x == "numeric") {
                # group and average
                DT.grouped <- popDT[, .(
                  mean = mean(get(i), na.rm = TRUE),
                  median = median(get(i), na.rm = TRUE),
                  sum = sum(get(i), na.rm = TRUE),
                  qUp = quantile(get(i), quantiles[[1]], na.rm = TRUE),
                  qLow = quantile(get(i), quantiles[[2]], na.rm = TRUE),
                  sd = sd(get(i), na.rm = TRUE)
                  ), by = .(track_id)]
                obsColNames <- c(
                  sprintf("%s.mean", i),
                  sprintf("%s.median", i),
                  sprintf("%s.sum", i),
                  sprintf("%s.qUp", i),
                  sprintf("%s.qLow", i),
                  sprintf("%s.sd", i)
                )
                
                # rename
                setnames(DT.grouped, "mean", obsColNames[[1]])
                setnames(DT.grouped, "median", obsColNames[[2]])
                setnames(DT.grouped, "sum", obsColNames[[3]])
                setnames(DT.grouped, "qUp", obsColNames[[4]])
                setnames(DT.grouped, "qLow", obsColNames[[5]])
                setnames(DT.grouped, "sd", obsColNames[[6]])
              }
              
              # merge to main DT
              tracksInfo[DT.grouped, on = .(track_id),
                         (obsColNames) := mget(obsColNames)]
              
              # add names to list
              obsCols <- c(obsCols, obsColNames)
            }
          }
          
          # trim dataset
          if (addPop == TRUE)
            tracksInfo <- tracksInfo[, mget(c("track_id", "pop", obsCols))]
          else
            tracksInfo <- tracksInfo[, mget(c("track_id", obsCols))]
          
          # replace NA
          if (replaceNA == TRUE)
            tracksInfo[is.na(tracksInfo)] <- 0
          
          # set value
          private$labelTracksInfo <- .setVersionedVar(
            private$labelTracksInfo, tracksInfo,
            valueName = versionedVarName
          )
        }
      }
      
      tracksInfo
    },
    
    #' @description Tracks measures
    #' @param pops list of character to define populations
    #' @param measures list of character to define measures
    #' @param extraMeasures list of character to define extra measures (not in celltrackR)
    #' @param forceReload boolean to force reload of values3
    #' @param ... passed to tracks
    tracksMeasures = function(pops, measures = NULL, extraMeasures = NULL, forceReload = FALSE, ...) {
      # get default measures if null
      if (is.null(measures)) {
        measures <- c(
          "speed", "duration", "trackLength", "meanTurningAngle",
          "displacement", "straightness", "displacementRatio",
          "outreachRatio", "asphericity", "overallAngle"
        )
      }
      
      versionedVarName <- paste(
        pops,
        paste(measures, collapse = "&"),
        paste(extraMeasures, collapse = "&"),
        sep = ":"
      )
      
      # create md5 hash
      versionedVarName <- digest::digest(versionedVarName)
      
      # was this requested before?
      tracksMeasures <- .getVersionedVar(
        private$labelTracksInfo, valueName = versionedVarName)
      
      if (is.null(tracksMeasures) || forceReload == TRUE) {
        tracks.DTs <- list()
        
        # ensure that pops has names
        if (is.null(names(pops))) {
          names(pops) <- pops
        }
        
        # get tracks
        tracks <- lapply(pops, function(x) self$tracks(pop = x, ...))
        
        # go through measurements
        for (measure.x in measures) {
          # get measurements
          tracks.DTs[[measure.x]] <- tracks.measure.fun(
            # tracks, get(measure.x), result.name = measure.x, idcol = "cell_type")
            tracks, eval(parse(text = paste0("celltrackR::", measure.x))),
            result.name = measure.x, idcol = "cell_type")
        }
        
        # go through extra measurements
        for (measure.x in extraMeasures) {
          # get measurements
          tracks.DTs[[measure.x]] <- tracks.measure.fun(
            # tracks, get(measure.x), result.name = measure.x, idcol = "cell_type")
            tracks, eval(parse(text = measure.x)),
            result.name = measure.x, idcol = "cell_type", ...)
        }

        if (all(lengths(tracks.DTs) > 0))
          tracksMeasures <- Reduce(function(...) merge(..., all = TRUE), tracks.DTs)
      }
      
      tracksMeasures
    },
    
    #' @description Tracks
    #' @param pop character to define population
    #' @param forceReload boolean to force reload of tracks
    #' @param minTracklength integer for minimum track length
    #' @param steps.subtracks integer for subtracks
    #' @param steps.overlap integer for subtracks overlap
    tracks = function(pop, forceReload = FALSE, minTracklength = 0,
                      steps.subtracks = NULL, steps.overlap = steps.subtracks - 1) {
      # was this requested before?
      tracks <- .getVersionedVar(
        private$labelTracks, valueName = pop)
      
      if (is.null(tracks) || forceReload == TRUE) {
        popMap <- self$imPopMap(
          "live", popPath = pop, tracksOnly = TRUE
        )
        
        # if the population is filtered
        # get the valueName
        if (length(popMap) > 0) {
          popMap <- popMap[[1]]
          
          valueName <- popMap$valueName
        } else {
          # ie/ get the DT from that non-tracks population
          valueName <- pop
        }
        
        # get population
        popDT <- self$popDT(
          "live", pops = valueName, popCols = c(
            "centroids", "track_id"
          ), dropNA = TRUE, includeFiltered = TRUE)
        
        if (!is.null(popDT) && "track_id" %in% colnames(popDT)){
          # get column definitions
          # celltrackR will do this:
          # colnames(r) <- c("id","t",c("x","y","z")[seq_along(pos.columns)])
          id.column <- which(colnames(popDT) == "track_id")
          time.column <- which(colnames(popDT) == "centroid_t")
          pos.columns.x <- which(colnames(popDT) == "centroid_x")
          pos.columns.y <- which(colnames(popDT) == "centroid_y")
          pos.columns.z <- which(colnames(popDT) == "centroid_z")
          pos.columns <- c(pos.columns.x, pos.columns.y, pos.columns.z)
          
          # convert to physical units for tracks
          suppressWarnings({
            tracks <- celltrackR::as.tracks(
              convertPixelToPhysical(popDT, self$omeXMLPixelRes()),
              id.column = id.column,
              time.column = time.column,
              pos.columns = pos.columns,
              scale.t = self$omeXMLTimelapseInfo()$interval
              # scale.p = (
              #   self$omeXMLPixelRes()$x * self$omeXMLPixelRes()$y * self$omeXMLPixelRes()$z
              # )
            )
          })
          
          # filter tracks by length
          if (minTracklength > 0) {
            tracks <- celltrackR::filterTracks(
              tracks.fun.time.filter,
              tracks,
              min.timepoints = minTracklength
            )
          }
          
          # filter tracks by defined filtering method
          if (!is.null(popMap) && "filterMeasure" %in% names(popMap)) {
            # add populations if frequency required
            addPops <- NULL
            if (popMap$filterMeasure == "pop") {
              addPops <- popMap$filterCategory
            }
            
            # get tracks info
            tracksInfo <- self$tracksInfo(
              popMap$filterMeasure,
              parentPop = popMap$parent,
              pop = pop,
              addPops = addPops
            )
            
            # get stats type
            statsType <- .cciaStatsType(popMap$filterMeasure)
            
            # add category to measure
            if ("filterCategory" %in% names(popMap) && !is.null(popMap$filterCategory)) {
              filterMeasure <- sprintf(
                "%s.%s", popMap$filterMeasure, popMap$filterCategory)
            } else {
              if (statsType == "numeric") {
                filterMeasure <- sprintf(
                  "%s.mean", popMap$filterMeasure)
              } else {
                filterMeasure <- sprintf(
                  "%s.freq", popMap$filterMeasure)
              }
            }
            
            # filter on value
            filtered <- FALSE
            if (filterMeasure %in% names(tracksInfo)) {
              if ("filterFun" %in% names(popMap)) {
                if (popMap$filterFun == "gt") {
                  tracksInfo <- tracksInfo[
                    get(filterMeasure) > popMap$filterValues, ]
                } else if (popMap$filterFun == "gte") {
                  tracksInfo <- tracksInfo[
                    get(filterMeasure) >= popMap$filterValues, ]
                } else if (popMap$filterFun == "lt") {
                  tracksInfo <- tracksInfo[
                    get(filterMeasure) < popMap$filterValues, ]
                } else if (popMap$filterFun == "lte") {
                  tracksInfo <- tracksInfo[
                    get(filterMeasure) <= popMap$filterValues, ]
                } else if (popMap$filterFun == "eq") {
                  tracksInfo <- tracksInfo[
                    get(filterMeasure) %in% popMap$filterValues, ]
                } else if (popMap$filterFun == "neq") {
                  tracksInfo <- tracksInfo[
                    !get(filterMeasure) %in% popMap$filterValues, ]
                }
              } else {
                tracksInfo <- tracksInfo[
                  get(filterMeasure) == popMap$filterValues, ]
              }
              
              filtered <- TRUE
            } 
            
            # filter tracks
            if (filtered == TRUE && nrow(tracksInfo) > 0) {
              tracks <- tracks[
                names(tracks) %in% tracksInfo$track_id]
            } else {
              if ("filterDefaultAll" %in% names(popMap)) {
                if (popMap$filterDefaultAll == FALSE) {
                  tracks <- list()
                }
              }
            }
          }
          
          # create subtracks?
          if (!is.null(steps.subtracks)) {
            tracks <- celltrackR::subtracks(
              tracks, i = steps.subtracks, overlap = steps.overlap)
          }
          
          # set value
          private$labelTracks <- .setVersionedVar(
            private$labelTracks, tracks, valueName = pop
          )
        }
      }
      
      tracks
    },
    
    #' @description Population stats
    #' @param popType character of population type
    popStats = function(popType) {
      # filter for population stats
      # TODO this returns all stats
      popStats <- data.table::rbindlist(
        lapply(
          self$valueNames("imLabelPropsFilepath"),
          function(x) {
            labelProps <- self$labelProps(valueName = x)
            
            stats <- NULL
            if (!is.null(labelProps)) {
              stats <- as.data.table(
                labelProps$values_obs())
              labelProps$close()
            }
              
            stats
          }),
        idcol = "valueName", fill = TRUE
      )
      
      popStatCols <- colnames(popStats)[
        startsWith(colnames(popStats),
                   paste0(popType, "."))
      ]
      
      # add labels
      popStatCols <- c("valueName", "label", popStatCols)
      
      popStats <- popStats[, ..popStatCols]
      
      popStats
    },
    
    #' @description property columns from labels
    #' @param valueNames list of character to define value names
    #' @param dataTypes list of character to define data types from adata
    #' @param popType character for population type
    #' @param colsStartsWith character to filter for values starting with
    #' @param trimCols character vector to trim values
    labelPropsCols = function(valueNames = NULL, dataTypes = c("vars", "obs"),
                              popType = NULL, colsStartsWith = NULL, trimCols = NULL) {
      # get value names
      if (is.null(valueNames)) {
        valueNames <- self$valueNames("imLabelPropsFilepath")
      }
      
      # return popCols
      propCols <- unique(
        unlist(lapply(
          valueNames,
          function(x) {
            labelProps <- self$labelProps(valueName = x)
            
            if (!is.null(labelProps)) {
              labelProps$change_channel_names(
                .flowCorrectChannelNames(unlist(self$imChannelNames()))
              )
              
              propsColNames <- unlist(sapply(
                dataTypes,
                function(y) labelProps$col_names(data_type = y)
                ))
              labelProps$close()
              
              propsColNames
            }
          }))
      )
      
      # filter for population type
      if (!is.null(popType)) {
        propCols <- propCols[startsWith(propCols, paste0(popType, "."))]
      }
      
      # filter for cols starting with
      if (!is.null(colsStartsWith)) {
        propCols <- propCols[startsWith(propCols, colsStartsWith)]
      }
      
      # trim columns?
      if (!is.null(trimCols)) {
        propCols <- sapply(
          trimCols, function(x) stringr::str_extract(propCols, paste0("(?<=", x, ").*")))
        propCols <- propCols[!is.na(propCols)]
      }
      
      propCols
    },
    
    #' @description Label props
    #' @param valueName character for value name
    #' @param forceReload boolean to force reload data
    #' @param readOnly boolean to load dataset read-only - not sure that works
    labelProps = function(valueName = NULL, forceReload = FALSE, readOnly = TRUE) {
      props <- NULL
      
      # was this requested before?
      propUtils <- .getVersionedVar(
        private$labelPropsUtils, valueName = valueName)
      
      if (is.null(propUtils) || forceReload == TRUE) {
        # check whether file exists
        if (!purrr::is_empty(self$imLabelPropsFilepath(valueName = valueName))) {
          if (file.exists(self$imLabelPropsFilepath(valueName = valueName))) {
            # load data
            propUtils <- cciaEnv()$LabelPropsUtils(
              self$persistentObjectDirectory(),
              self$imLabelPropsFilepath(valueName = valueName, absolutePath = FALSE)
            )
            
            # set value
            private$labelPropsUtils <- .setVersionedVar(
              private$labelPropsUtils, propUtils, valueName = valueName
            )
          }
        }
      }
      
      if (!is.null(propUtils)) {
        # return view and add channel names
        propsView <- propUtils$label_props_view(
          read_only = readOnly)
        
        propsView$change_channel_names(
          .flowCorrectChannelNames(unlist(self$imChannelNames()))
        )
        
        props <- propsView
      }
      
      props
    },
    
    #' @description Region props
    #' @param valueName character for value name
    #' @param forceReload boolean to force reload data
    #' @param readOnly boolean to load dataset read-only - not sure that works
    regionProps = function(valueName = NULL, forceReload = FALSE, readOnly = TRUE) {
      props <- NULL
      
      # was this requested before?
      propUtils <- .getVersionedVar(
        private$regionPropsUtils, valueName = valueName)
      
      if (is.null(propUtils) || forceReload == TRUE) {
        # check whether file exists
        if (!purrr::is_empty(self$imRegionsFilepath(valueName = valueName))) {
          if (file.exists(self$imRegionsFilepath(valueName = valueName))) {
            # load data
            propUtils <- cciaEnv()$LabelPropsUtils(
              self$persistentObjectDirectory(),
              self$imRegionsFilepath(valueName = valueName, absolutePath = FALSE)
            )
            
            # set value
            private$regionPropsUtils <- .setVersionedVar(
              private$regionPropsUtils, propUtils, valueName = valueName
            )
          }
        }
      }
      
      if (!is.null(propUtils)) {
        # return view and add channel names
        propsView <- propUtils$label_props_view(
          read_only = readOnly)
        
        propsView$change_channel_names(
          .flowCorrectChannelNames(unlist(self$imChannelNames()))
        )
        
        props <- propsView
      }
      
      props
    },
    
    #' @description Neighbour props
    #' @param valueName character for value name
    #' @param forceReload boolean to force reload data
    #' @param readOnly boolean to load dataset read-only - not sure that works
    neighbourProps = function(valueName = NULL, forceReload = FALSE, readOnly = TRUE) {
      props <- NULL
      
      # was this requested before?
      propUtils <- .getVersionedVar(
        private$neighourPropsUtils, valueName = valueName)
      
      if (is.null(propUtils) || forceReload == TRUE) {
        # check whether file exists
        if (!purrr::is_empty(self$imNeighboursFilepath(valueName = valueName))) {
          if (file.exists(self$imNeighboursFilepath(valueName = valueName))) {
            # load data
            propUtils <- cciaEnv()$LabelPropsUtils(
              self$persistentObjectDirectory(),
              self$imNeighboursFilepath(valueName = valueName, absolutePath = FALSE)
            )
            
            # set value
            private$neighourPropsUtils <- .setVersionedVar(
              private$neighourPropsUtils, propUtils, valueName = valueName
            )
          }
        }
      }
      
      if (!is.null(propUtils)) {
        # return view and add channel names
        propsView <- propUtils$label_props_view(
          read_only = readOnly)
        
        propsView$change_channel_names(
          .flowCorrectChannelNames(unlist(self$imChannelNames()))
        )
        
        props <- propsView
      }
      
      props
    },
    
    #' @description Population object
    #' @param popType character for population type
    #' @param valueName character for value name
    #' @param ... passed to population utils object
    popUtils = function(popType, valueName = NULL, ...) {
      retVal <- NULL
      
      if (popType == "flow") {
        retVal <- self$flowGatingSet(...)
      } else if (popType == "clust") {
        retVal <- self$adataUtils(popType = popType, ...)
      } else if (popType == "region") {
        retVal <- self$adataUtils(
          popType = popType,
          adataPath = self$imRegionsFilepath(valueName = valueName), ...)
      } else if (popType == "live") {
        retVal <- self$livePopUtils(...)
      } else if (popType == "clsf") {
        retVal <- self$clsfPopUtils(...)
      } else if (popType == "branch") {
        retVal <- self$branchPopUtils(...)
      } else if (popType == "labels") {
        retVal <- self$labelsPopUtils(...)
      }
      
      retVal
    },
    
    #' @description All population paths as list
    #' @param flattenPops boolean to flatten pops
    #' @param useNames boolean to use names instead of uIDs
    #' @param ... passed to self$popPaths
    popPathsAll = function(flattenPops = FALSE, useNames = FALSE, ...) {
      # go through all pop types and get paths
      popPaths <- names(cciaConf()$parameters$popTypes)
      names(popPaths) <- popPaths
      
      popResults <- lapply(
        popPaths, function(x) {
          self$popPaths(x, ...)
        })
      
      # flatten pops
      if (flattenPops == TRUE) {
        popResults <- popResults[lengths(popResults) > 0]
        
        # flatten down per pop type
        popResults <- mapply(
          function(x, i) {
            x <- paste(i, unlist(x), sep = ".")
            
            # use names instead of uIDs
            if (useNames == TRUE)
              names(x) <- x
            
            as.list(x)
          }, popResults, names(popResults), SIMPLIFY = FALSE
        )
      }
      
      popResults
    },
    
    #' @description Population paths
    #' add populations from population map
    #' if there were filtered
    #' @param popType character for population type
    #' @param includeFiltered boolean to include filtered populations
    #' @param filteredOnly boolean to only return filtered populations
    #' @param includeRoot boolean to include root
    #' @param tracksOnly boolean to return tracks only
    #' @param cellsOnly boolean to return cells only
    #' @param parentPops list of character of parent populations
    #' @param filterMeasures list of character to filter on filtering measures
    #' @param ... passed to self$popUtils
    popPaths = function(popType, includeFiltered = FALSE,
                        filteredOnly = FALSE, includeRoot = FALSE,
                        # tracksOnly = FALSE, cellsOnly = TRUE,
                        tracksOnly = FALSE, cellsOnly = FALSE,
                        parentPops = NULL, filterMeasures = NULL, ...) {
      popUtils <- self$popUtils(popType = popType, ...)
      
      popPaths <- list()
      
      if (!is.null(popUtils)) {
        # get population paths from utils
        popPaths <- self$popUtils(popType = popType, ...)$popPaths(
          includeRoot = includeRoot
        )
        
        # set include filtered
        if (filteredOnly == TRUE) {
          includeFiltered = TRUE
        }
        
        popIDs <- NULL
        
        # add populations from map if filtered
        if (includeFiltered == TRUE) {
          # use tracks only?
          if (tracksOnly == TRUE) {
            popIDs <- self$popIDsByAttr(
              popType, "isTrack", TRUE, includeFiltered = TRUE)
          } else if (cellsOnly == TRUE) {
            popIDs <- self$popIDsByAttr(
              popType, "isTrack", FALSE, includeFiltered = TRUE)
          } else {
            popIDs <- names(popPaths)
          }
          
          # get filtered pops
          filteredPops <- sapply(
            self$imPopMap(popType, popIDs = popIDs, includeFiltered = TRUE, filterMeasures = filterMeasures),
            function(x) if (any(c("filterMeasure", "filterMeasures") %in% names(x))) x$path
          )
          
          # add to paths
          if (filteredOnly == FALSE) {
            popPaths <- c(popPaths, filteredPops)
          } else {
            popPaths <- filteredPops
          }
          
          # get pop ids
          if (is.null(popIDs))
            popIDs <- names(popPaths)
        }
        
        # filter NULL
        # TODO don't get NULL in the first place
        popPaths <- popPaths[lengths(popPaths) > 0]
        
        # get IDs for pops
        if (length(popPaths) > 0 && !is.null(names(popPaths))) {
          names(popPaths)[
            # !unlist(sapply(popPaths, flowPopIsRoot, simplify = FALSE))
            !unlist(sapply(names(popPaths), function(x) is.null(x) || x == "",
                           simplify = FALSE))
            ] <- popIDs
        }
        
        popMapPaths <- self$popAttr(
          popType, "path",
          popIDs = popIDs,
          includeFiltered = includeFiltered)
        
        # merge with IDs
        popPaths <- append(
          popPaths[!popPaths %in% popMapPaths],
          popMapPaths
        )
        
        # set name if they don't have one
        if (is.null(names(popPaths))) {
          names(popPaths) <- popPaths
        } else if (any(names(popPaths) == "")) {
          names(popPaths)[names(popPaths) == ""] <- popPaths[names(popPaths) == ""]
        }
      }
      
      # return only pop paths that are children of parent
      if (length(parentPops) > 0) {
        
      }
      
      popPaths
    },
    
    #' @description Neighbours DT
    #' TODO could this be done easier?
    #' @param ... passed to self$neighbourProps
    neighboursDT = function(...) {
      self$neighbourProps(...)$as_df() %>%
        as.data.table
    },
    
    #' @description Population datatable
    #' apply filtering populations if needed
    #' TODO this got a bit complicated - can you clean this up?
    #' @param popType character for population type
    #' @param pops list of character for populations
    #' @param popCols list of character for columns to include
    #' @param dropNA boolean to drop NA
    #' @param dropPop boolean to drop population
    #' @param includeFiltered boolean to include filtered population
    #' @param forceReload boolean to force reload data
    #' @param uniqueLabels boolean to force unique labels
    #' @param flushCache boolean to flush cache
    #' @param replaceNA boolean to replace NA
    #' @param completeDT boolean to complete data.table with label props
    #' @param filterMeasures list of character to include filter measures
    #' @param includeX boolean to include 'X' from adata
    #' @param replaceX boolean to replace 'X' with 'X' from adata
    #' @param includeObs boolean to include 'obs' from adata
    #' @param completeValueNames character for value names to complete DT
    #' @param completePops boolean to complete pops
    #' @param tracksOnly boolean to get tracks only
    #' @param ... passed to self$popUtils
    popDT = function(popType, pops = NULL, popCols = NULL,
                     dropNA = FALSE, dropPop = FALSE, includeFiltered = FALSE,
                     forceReload = FALSE, uniqueLabels = TRUE,
                     flushCache = FALSE, replaceNA = FALSE,
                     completeDT = TRUE, filterMeasures = NULL,
                     includeX = FALSE, replaceX = FALSE, includeObs = TRUE, 
                     completeValueNames = c(), completePops = TRUE,
                     tracksOnly = FALSE, ...) {
      # make sure label is in columns
      if (!is.null(popCols)) {
        if (!"label" %in% popCols)
          popCols <- c("label", popCols)
        if (!"pop" %in% popCols)
          popCols <- c("pop", popCols)
      }
      
      # set value name for versioned variable
      # TODO this will save a DT for specific requested
      # columns, that is a bit of an overhead ..
      versionedVarName <- paste(
        popType,
        completeDT,
        if (!is.null(pops)) pops else "root",
        filterMeasures,
        sep = ":"
      )
      
      if (!is.null(popCols)) {
        versionedVarName <- paste(
          versionedVarName,
          paste(popCols, collapse = "&"),
          sep = ":"
        )
      }
      
      # create md5 hash
      versionedVarName <- digest::digest(versionedVarName)
      
      # flush cache for populations
      if (flushCache == TRUE) {
        private$filteredPopDT <- .setVersionedVar(
          private$filteredPopDT, NULL,
          valueName = versionedVarName
        )
        
        # only do that once
        flushCache <- FALSE
      }
      
      prevFilteredPopDT <- NULL
      
      # get population utils
      popUtils <- self$popUtils(popType = popType, ...)
      
      popsPresent <- TRUE
      
      # get all populations if not set
      nonFilteredPops <- NULL
      
      if (completePops == TRUE && (is.null(pops) || .flowPopIsRoot(pops))) {
        pops <- self$popPaths(popType, includeFiltered = includeFiltered,
                              includeRoot = FALSE, tracksOnly = tracksOnly, ...)
        
        # set non-filtered populations
        nonFilteredPops <- self$popPaths(
          popType, includeFiltered = FALSE, includeRoot = TRUE, tracksOnly = tracksOnly, ...)
      } else {
        # check that any pops are available
        # popsPresent <- all(pops %in% self$popPaths(popType, includeFiltered = includeFiltered))
        popsPresent <- any(pops %in% self$popPaths(
          popType, includeFiltered = includeFiltered, includeRoot = TRUE, tracksOnly = tracksOnly, ...))
      }
      
      if (popsPresent == TRUE) {
        # get population IDs
        popIDs <- self$popIDsByAttr(
          popType, "path", pops, compareFun = "in",
          includeFiltered = includeFiltered
        )
        popMap <- self$imPopMap(
          popType, includeFiltered = includeFiltered)
        popMapPops <- popMap[names(popMap) %in% popIDs]
        
        # get non-filtered populations
        if (is.null(nonFilteredPops)) {
          nonFilteredPops <- self$popPaths(
            popType, includeFiltered = FALSE, includeRoot = TRUE, ...)
          
          # filter for selected pops
          nonFilteredPops <- nonFilteredPops[nonFilteredPops %in% pops]
        }
        
        # get non-filtered populations
        popDT <- NULL
        
        if (length(nonFilteredPops) > 0) {
          popDT <- popUtils$popDT(
            pops = nonFilteredPops, popCols = popCols,
            dropNA = dropNA, dropPop = dropPop)
          
          # complete DT
          if (completeDT == TRUE)
            popDT <- private$completePopDT(
              popUtils, popDT, popCols = popCols, uniqueLabels = uniqueLabels,
              includeX = includeX, replaceX = replaceX, includeObs = includeObs,
              valueNames = completeValueNames)
          
          # make sure columns are unique
          # TODO this should actually not happen
          # popDT <- popDT[, .SD, .SDcols = unique(names(popDT))]
        }
        
        # build list
        filteredDTs <- list()
        
        # get filtered populations
        filteredPops <- lapply(popMapPops, function(x)
          if (any(c("filterMeasure", "filterMeasures") %in% names(x))) x)
        filteredPopPaths <- sapply(popMap, function(x)
          if (any(c("filterMeasure", "filterMeasures") %in% names(x))) x$path)
        
        # filter NULL
        # TODO not very clean
        filteredPops <- filteredPops[lengths(filteredPops) > 0]
        filteredPopPaths <- filteredPopPaths[lengths(filteredPopPaths) > 0]
        
        # go through
        for (x in filteredPops) {
          filteredPopDT <- NULL
          
          # one or multiple filter measures?
          filterMeasureName <- if ("filterMeasure" %in% names(x))
            "filterMeasure"
          else
            "filterMeasures"
          
          # if (forceReload == FALSE) {
          # get previously requested filtered populations
          prevFilteredPopDT <- .getVersionedVar(
            private$filteredPopDT, valueName = versionedVarName)
          
          # was the population requested before?
          if (!is.null(prevFilteredPopDT) && nrow(prevFilteredPopDT) > 0) {
            filteredPopDT <- prevFilteredPopDT[pop == x$path, ]
            
            if (nrow(filteredPopDT) > 0) {
              filteredDTs[[x$path]] <- filteredPopDT
              
              message(sprintf(">> Get %s from previous call", x$path))
            } else {
              filteredPopDT <- NULL
            }
          }
          # }
          
          showFilteredPop <- TRUE
          
          # should this population be shown?
          if (length(filterMeasures) > 0 && !any(x[[filterMeasureName]] %in% filterMeasures))
            showFilteredPop <- FALSE
          
          if (is.null(filteredPopDT) && showFilteredPop == TRUE) {
            # is the parent a filtered population?
            parentIsFiltered <- x$parent %in% filteredPopPaths
            
            # add filter measure to columns
            if (!is.null(popCols)) {
              if (!any(x[[filterMeasureName]] %in% popCols)) {
                popCols <- c(popCols, x[[filterMeasureName]][!x[[filterMeasureName]] %in% popCols])
              }
            }
            
            # get population DT
            if (parentIsFiltered == TRUE) {
              filteredPopDT <- self$popDT(
                popType, x$parent, popCols = popCols,
                dropNA = dropNA, dropPop = dropPop,
                completeDT = completeDT, replaceNA = replaceNA,
                includeFiltered = TRUE)
            } else {
              # get pop utils for filtered
              filteredPopUtils <- self$popUtils(popType)
              
              filteredPopDT <- filteredPopUtils$popDT(
                x$parent, popCols = popCols, dropNA = dropNA, dropPop = dropPop)
              
              # complete DT
              if (completeDT == TRUE)
                filteredPopDT <- private$completePopDT(
                  filteredPopUtils, filteredPopDT, popCols = popCols,
                  uniqueLabels = uniqueLabels,
                  includeX = includeX, replaceX = replaceX, includeObs = includeObs,
                  valueNames = completeValueNames)
              
              # make sure columns are unique
              # TODO this should actually not happen
              # filteredPopDT <- filteredPopDT[, .SD, .SDcols = unique(names(filteredPopDT))]
            }
            
            # is the filter value present?
            if (any(x[[filterMeasureName]] %in% names(filteredPopDT))) {
              # one or multiple filter functions?
              filterFunName <- NULL
              filterFunName <- if ("filterFun" %in% names(x))
                "filterFun"
              else if ("filterFuns" %in% names(x))
                "filterFuns"
              
              if (!is.null(filterFunName)) {
                filteredDTs[[x$path]] <- .popsApplyFilterToPopDT(x, filteredPopDT)
              } else {
                # go through filters
                j <- 1
                for (y in x[[filterMeasureName]]) {
                  if (filterMeasureName == "filterMeasures") {
                    filteredDTs[[x$path]] <- filteredPopDT[
                      get(y) == x$filterValues, ]
                  } else {
                    filteredDTs[[x$path]] <- filteredPopDT[
                      get(y) == x$filterValues[[j]], ]
                    j <- j + 1
                  }
                }
              }
              
              if (nrow(filteredDTs[[x$path]]) > 0) {
                # add population name
                filteredDTs[[x$path]]$pop <- x$path
              }
            } else {
              # add all cells by default
              if ("filterDefaultAll" %in% names(x)) {
                if (x$filterDefaultAll == TRUE) {
                  # if (!is.null(filteredPopDT) && nrow(filteredPopDT) > 0) {
                  if (length(filteredPopDT) > 0 && nrow(filteredPopDT) > 0) {
                    filteredDTs[[x$path]] <- filteredPopDT
                    
                    # add population name
                    filteredDTs[[x$path]]$pop <- x$path
                  }
                }
              }
            }
            
            if (!is.null(filteredDTs[[x$path]])) {
              # set filtered value
              private$filteredPopDT <- .setVersionedVar(
                private$filteredPopDT,
                if (!is.null(prevFilteredPopDT)) {
                  rbind(filteredDTs[[x$path]], prevFilteredPopDT, fill = TRUE)
                } else {
                  filteredDTs[[x$path]]
                },
                valueName = versionedVarName
              )
              
              # message(sprintf(">> save popDT %s", versionedVarName))
            }
          }
        }
        
        # bind together
        if (length(filteredDTs) > 0) {
          # compile DT
          if (uniqueLabels == FALSE) {
            # simply bind together
            popDT <- data.table::rbindlist(append(list(popDT), filteredDTs),
                               fill = TRUE)
          } else {
            # TODO is there a better way to do this?
            # compile filtered populations
            # sort names by number of division paths
            filteredPopNames <- names(filteredDTs)
            names(filteredPopNames) <- filteredPopNames
            filteredPopNames <- sort(sapply(filteredPopNames, function(x) lengths(stringr::str_split(x, "/"))))
            filteredPopNames <- names(filteredPopNames)
            filteredPopNames <- unname(filteredPopNames)
            
            filteredDT <- filteredDTs[[filteredPopNames[1]]]
            
            if (length(filteredDTs) > 1) {
              for (y in filteredPopNames[2:length(filteredPopNames)]) {
                # TODO is there a better way to do this .. ?
                mergeCols <- intersect(colnames(filteredDT), colnames(filteredDTs[[y]]))
                mergeCols <- mergeCols[mergeCols != "pop"]
                
                filteredDT <- merge(
                  # this assumes that all requested populations
                  # have a common parent which is also requested
                  # filteredDT, filteredDTs[[y]][, c("value_name", "label", "pop")],
                  # by = c("value_name", "label"),
                  # all.x = TRUE
                  # )[is.na(pop.y), pop.y := pop.x][, pop := pop.y][, pop.x := NULL][, pop.y := NULL]
                  filteredDT, filteredDTs[[y]],
                  # TODO this seems very non-efficient
                  by = mergeCols,
                  # would that help somehow .. ?
                  # no.dups = FALSE,
                  all = TRUE
                )
                
                # rename pop if necessary
                if ("pop.y" %in% colnames(filteredDT)) {
                  filteredDT[is.na(pop.y), pop.y := pop.x][, pop := pop.y][, pop.x := NULL][, pop.y := NULL]
                }
              }
            }
            
            # merge names to non-filtered pops
            # https://stackoverflow.com/a/33954334/13766165
            if (!is.null(popDT)) {
              # make sure popCols are in DT
              mergeCols <- c("uID", "value_name", "label", "track_id")
              mergeCols <- mergeCols[mergeCols %in% names(popDT)]
              mergeColsFiltered <-  c(mergeCols, "pop")
              
              popDT <- merge(
                popDT, filteredDT[, ..mergeColsFiltered],
                by = mergeCols,
                all.x = TRUE
              )[is.na(pop.y), pop.y := pop.x][, pop := pop.y][, pop.x := NULL][, pop.y := NULL]
              
              # compile together - this is needed if there
              # are filtered populations but the respective
              # non-filtered population was not requested
              # TODO is this really necessary?
              # or should I just always return non-filtered pops ..?
              popDT <- merge(
                popDT, filteredDT,
                by = intersect(colnames(popDT), colnames(filteredDT)),
                all = TRUE)
            } else {
              popDT <- filteredDT
            }
          }
        }
        
        # make sure that pop levels are ok
        if (is.factor(popDT[, pop]))
          popDT[, pop := droplevels(pop)]
        
        # replace NA with 0
        if (replaceNA == TRUE)
          popDT[is.na(popDT)] <- 0
        
        return(popDT)
      } else {
        return(NULL)
      }
    },
    
    #' @description Save population labels to disk
    #' python can use that to filter
    #' the adata files wihtout being
    #' aware of the populations themselves
    #' @param popType character for population type
    #' @param pops list of character for populations
    #' @param includeFiltered boolean to include filtered population
    #' @param flushCache boolean to flush cache
    #' @param completeDT boolean to complete data.table with label props
    #' @param tracksOnly boolean to save tracks only
    #' @param ... passed to self$popUtils(popType = popType)$savePops
    savePops = function(popType, pops = NULL, includeFiltered = FALSE,
                        flushCache = TRUE, completeDT = TRUE, tracksOnly = FALSE,
                        ...) {
      # get pops
      if (is.null(pops) || .flowPopIsRoot(pops)) {
        # pops <- sapply(self$imPopMap(popType = popType,
        #                              includeFiltered = includeFiltered),
        #                function(x) x$path)
        pops <- self$popPaths(popType, includeFiltered = includeFiltered)
      }
      
      # add pop ids if not given
      if (is.null(names(pops))) {
        popIDs <- self$popIDsFromPaths(
          self$imPopMap(popType = popType, includeFiltered = includeFiltered),
          pops
        )
        
        if (length(popIDs) > 0) {
          names(pops) <- pops
          names(pops)[pops %in% names(popIDs)] <- popIDs[pops][!is.na(popIDs[pops])]
        }
      }
      
      # TODO to get a population - get only one column for faster retrieval
      # popCols <- c(self$imChannelNames()[1], "label")
      popCols <- c(self$imChannelNames()[1], "label", "track_id", "value_name", "uID")
      
      # get population DT
      popDT <- self$popDT(popType = popType, pops = pops, popCols = popCols,
                          includeFiltered = includeFiltered,
                          uniqueLabels = FALSE, flushCache = flushCache,
                          # completeDT = completeDT)
                          completeDT = includeFiltered, tracksOnly = tracksOnly)
      
      # TODO make sure that only tracks from the current image are saved
      if (all(c("track_id", "uID") %in% colnames(popDT)))
        popDT <- popDT[uID == self$getUID()]
      
      if (!is.null(popDT)) {
        # add pop column if not present
        if (!"pop" %in% colnames(popDT)) {
          popDT$pop <- NA
        }
        
        # save
        self$popUtils(popType = popType)$savePops(
          pops = pops,
          popsDir = file.path(
            self$persistentObjectDirectory(),
            cciaConf()$dirs$tasks$populations, popType
          ),
          popDT = popDT, ...)
      }
    },
    
    #' @description pp3 object
    #' @param windowPops list of character to define window
    #' @param pops list of character to define populations
    #' @param usePops list of character to filter population data.frame
    #' @param usePhysicalScale boolean to convert to physical scale
    #' @param ... passed to self$popDT
    pp3 = function(windowPops = NULL, pops = NULL, usePops = NULL, usePhysicalScale = TRUE, ...) {
      # get popDT
      popDT <- self$popDT(pops = pops, ...)
      
      # filter on populations to use
      if (!is.null(usePops) && length(usePops) > 0) {
        popDT <- popDT[pop %in% usePops]
      }
      
      if (usePhysicalScale == TRUE)
        convertPixelToPhysical(popDT, self$omeXMLPixelRes())
      
      # get convex hull population
      if (length(windowPops) > 0) {
        popWindow <- self$popDT(pops = windowPops, ...)
        
        if (usePhysicalScale == TRUE)
          convertPixelToPhysical(popWindow, self$omeXMLPixelRes())
      } else {
        popWindow <- popDT
      }
      
      # create 3D spatial dataframe
      pointsDF <- data.frame(
        x = popDT$centroid_x,
        y = popDT$centroid_y,
        z = popDT$centroid_z
      )
      windowDF <- data.frame(
        x = popWindow$centroid_x,
        y = popWindow$centroid_y,
        z = popWindow$centroid_z
      )
      
      # create bbox
      # TODO should this be a polygon?
      if (nrow(pointsDF) > 0) {
        # create bbox window
        W <- spatstat.geom::box3(
          xrange = c(floor(windowDF$x), ceiling(windowDF$x)),
          yrange = c(floor(windowDF$y), ceiling(windowDF$y)),
          zrange = c(floor(windowDF$z), ceiling(windowDF$z))
        )
        
        spatstat.geom::as.ppp(pointsDF, marks = factor(popDT$pop), W = W)
      } else {
        NULL
      }
    },
    
    #' @description ppp
    #' https://stackoverflow.com/a/39956181
    #' @param windowPops list of character to define window
    #' @param pops list of character to define populations
    #' @param usePops list of character to filter population data.frame
    #' @param usePhysicalScale boolean to convert to physical scale
    #' @param hullType character to define type of hull
    #' @param concavity numeric to define concavity for concave hull
    #' @param ... passed to self$popDT
    ppp = function(windowPops = NULL, pops = NULL, usePops = NULL,
                   usePhysicalScale = TRUE, hullType = "convex",
                   concavity = 2, ...) {
      # get popDT
      popDT <- self$popDT(pops = pops, ...)
      
      # filter on populations to use
      if (length(usePops) > 0) {
        popDT <- popDT[pop %in% usePops]
      }
      
      if (usePhysicalScale == TRUE)
        convertPixelToPhysical(popDT, self$omeXMLPixelRes())
      
      # get convex hull population
      if (length(windowPops) > 0) {
        popWindow <- self$popDT(pops = windowPops, ...)
        
        if (usePhysicalScale == TRUE)
          convertPixelToPhysical(popWindow, self$omeXMLPixelRes())
      } else {
        popWindow <- popDT
      }
      
      # create 2D spatial dataframe
      pointsDF <- data.frame(
        x = popDT$centroid_x,
        y = popDT$centroid_y
      )
      windowDF <- data.frame(
        x = popWindow$centroid_x,
        y = popWindow$centroid_y
      )
      
      # create rectangle
      # TODO should this be a polygon?
      # get pixel info
      # omePixels <- self$omeXMLPixels()
      # W <- spatstat.geom::owin(c(0, omePixels$SizeX), c(0, omePixels$SizeY))
      
      if (nrow(pointsDF) > 0) {
        # create polygon window
        if (hullType == "concave") {
          # https://stackoverflow.com/a/33299920
          polyCoords <- concaveman::concaveman(as.matrix(windowDF), concavity = concavity)
          # reverse coordinates for owin ie/ anti-clockwise
          polyCoords <- polyCoords[nrow(polyCoords):1, ]
          
          W <- spatstat.geom::owin(poly = list(x = polyCoords[,1], y = polyCoords[,2]))
        } else {
          polyCoords <- windowDF[chull(as.matrix(windowDF)),] %>%
            arrange(desc(row_number()))
          W <- spatstat.geom::owin(poly = list(x = polyCoords$x, y = polyCoords$y))
        }
        
        spatstat.geom::as.ppp(pointsDF, marks = factor(popDT$pop), W = W)
      } else {
        NULL
      }
    },
    
    #' @description Spatial DT
    #' @param valueName character for value name
    #' @param forceReload boolean to force reload
    spatialDT = function(valueName = "default", forceReload = FALSE) {
      # was this requested before?
      spatialDT <- .getVersionedVar(
        private$handleSpatialDT, valueName = valueName)
      
      if (is.null(spatialDT) || forceReload == TRUE) {
        # get labels view and connectivities
        labelView <- self$neighbourProps(valueName = valueName)
        
        if (!is.null(labelView)) {
          # TODO clean up
          spatialDT <- as.data.table(labelView$as_spatial_connectivities())
          popDT <- as.data.table(labelView$values_obs())
          popDT[, id := (1:.N) - 1]
          labelView$close()
          
          if (!is.null(spatialDT) && length(spatialDT) > 0) {
            # TODO this is a bad fix if vertices are missing in popDT
            spatialDT <- spatialDT[
              spatialDT$from %in% popDT$id,]
            spatialDT <- spatialDT[
              spatialDT$to %in% popDT$id,]
            
            # add labels to spatial
            spatialDT[popDT[, c("id", "label")],
                      on = .(from = id), from := label]
            spatialDT[popDT[, c("id", "label")],
                      on = .(to = id), to := label]
            
            # set value
            private$handleSpatialDT <- .setVersionedVar(
              private$handleSpatialDT, spatialDT,
              valueName = valueName
            )
          }
        }
      }
      
      spatialDT
    },
    
    #' @description Spatial igraph
    #' @param popType character for population type
    #' @param valueName character for value name
    #' @param ... passed to self$spatialDT
    spatialGraph = function(popType, valueName = "default", ...) {
      # was this requested before?
      spatialGraph <- .getVersionedVar(
        private$handleSpatialGraph, valueName = valueName)
      
      if (is.null(spatialGraph) || forceReload == TRUE) {
        spatialDT <- self$spatialDT(valueName = valueName, ...)
        popDT <- self$popDT(popType,
                            # popCols = c("label", "pop"),
                            includeFiltered = TRUE)
        setnames(popDT, "label", "name")
        
        if (!is.null(spatialDT)) {
          # make graph
          spatialGraph <- igraph::graph_from_data_frame(
            spatialDT,
            directed = FALSE,
            vertices = popDT[, c("name", "pop")]
          )
          
          # set value
          private$handleSpatialGraph <- .setVersionedVar(
            private$handleSpatialGraph, spatialGraph,
            valueName = valueName
          )
        }
      }
      
      spatialGraph
    },
    
    #' @description FlowFrame
    #' @param valueName character for value name
    #' @param compensateZ boolean to compensate depth
    #' @param applyReversedLog boolean to apply reverse log
    #' @param reversedLogBase integer for base of reverse log
    #' @param ... passed to .flowCompensatePoly
    flowFrame = function(
      valueName, compensateZ = FALSE, applyReversedLog = FALSE,
      reversedLogBase = 0, ...) {
      # forceRelaoad = FALSE, init = TRUE) {
      # get label properties
      labelsView <- self$labelProps(valueName = valueName)
      
      # get extra columns
      if (length(labelsView$channel_types()) > 0) {
        extraChannelColumns <- unlist(lapply(
          labelsView$channel_types(), function(x) {
            labelsView$channel_columns(prefix = x)
          }))
      }
      
      # get dataframe
      labelProps <- as.data.table(labelsView$view_label_col()$as_df())
      labelsView$close()
      
      # compensate Z if selected
      if (compensateZ == TRUE) {
        labelProps <- .flowCompensatePoly(
          labelProps, self$imChannelNames(includeTypes = TRUE),
          "centroid_z", replaceValues = TRUE, ...)
      }
      
      # take reversed log
      if (applyReversedLog == TRUE) {
        logBase <- reversedLogBase
        
        # set natural log
        if (logBase == 0)
          logBase <- exp(1)
        
        # go through channels and take double reverse log
        for (x in self$imChannelNames(includeTypes = TRUE, correctChannelNames = TRUE)) {
          if (x %in% colnames(labelProps)) {
            # take log of reverse
            # avoid -inf
            labelProps[, c(x) := log((-get(x) + max(get(x))) + 1, base = logBase)]
            
            # reverse back
            labelProps[, c(x) := -get(x) + max(get(x))]
          }
        }
      }
      
      # create attributes to add
      propsToAdd <- c(
        # extraChannelColumns,
        cciaConf()$fcs$propsToAdd
      )
      
      # get channels
      imChannels <- self$imChannelNames(includeTypes = TRUE)
      
      # crreate flow frame
      .prepareFlowFrame(
        labelProps,
        imChannels,
        attrNames = propsToAdd,
        # channelPattern = cciaConf()$files$labelPropsChannels,
        channelPattern = paste0(attr(imChannels, "measure"), "_intensity"),
        addRownames = TRUE)
    },
    
    #' @description Gating set
    #' @param forceReload boolean to force reload data
    #' @param init boolean to init data
    flowGatingSet = function(forceReload = FALSE, init = TRUE) {
      if (init == TRUE) {
        if (!purrr::is_empty(self$imGatingSetFilepath())) {
          if (is.null(private$handleFlowGatingSet) || forceReload == TRUE) {
            if (file.exists(self$imGatingSetFilepath())) {
              # init object
              private$handleFlowGatingSet <- FlowGatingSet$new(
                self$imGatingSetFilepath(), self$imChannelNames(includeTypes = TRUE),
                # TODO if there are multiple gating set value names
                # this would need to be passed in the function call
                # and also here
                attr(self$valueNames("imGatingSetFilepath"), "default")
                )
              
              # init reactivity
              if (private$isReactive()) {
                private$handleFlowGatingSet$reactive()
              }
            }
          }
        }
      }
      
      private$handleFlowGatingSet
    },
    
    #' @description Classification utils
    #' @param forceReload boolean to force reload data
    #' @param init boolean to init data
    clsfPopUtils = function(forceReload = FALSE, init = TRUE) {
      if (init == TRUE) {
        if (!is.null(self$valueNames("imLabelPropsFilepath"))) {
          if (is.null(private$handleClsfPopUtils) || forceReload == TRUE) {
            valueNames <- self$valueNames("imLabelPropsFilepath", valueType = "clsf")
            
            # init object
            private$handleClsfPopUtils <- MultifileLabelPopUtils$new(
              self$persistentObjectDirectory(),
              # only classification labels
              valueNames[!is.na(stringr::str_match(valueNames, "\\.cl$"))],
              self$imChannelNames(includeTypes = TRUE)
            )
            
            # init reactivity
            if (private$isReactive()) {
              private$handleClsfPopUtils$reactive()
            }
          }
        }
      }
      
      private$handleClsfPopUtils
    },
    
    #' @description Labels utils
    #' @param forceReload boolean to force reload data
    #' @param init boolean to init data
    labelsPopUtils = function(forceReload = FALSE, init = TRUE) {
      if (init == TRUE) {
        if (!is.null(self$valueNames("imLabelPropsFilepath"))) {
          if (is.null(private$handleLabelsPopUtils) || forceReload == TRUE) {
            # init object
            private$handleLabelsPopUtils <- MultifileLabelPopUtils$new(
              self$persistentObjectDirectory(),
              self$valueNames("imLabelPropsFilepath", valueType = "labels"),
              self$imChannelNames(includeTypes = TRUE)
            )
            
            # init reactivity
            if (private$isReactive()) {
              private$handleLabelsPopUtils$reactive()
            }
          }
        }
      }
      
      private$handleLabelsPopUtils
    },
    
    #' @description Branching utils
    #' @param forceReload boolean to force reload data
    #' @param init boolean to init data
    branchPopUtils = function(forceReload = FALSE, init = TRUE) {
      if (init == TRUE) {
        if (!is.null(self$valueNames("imLabelPropsFilepath"))) {
          if (is.null(private$handleBranchPopUtils) || forceReload == TRUE) {
            valueNames <- self$valueNames("imLabelPropsFilepath", valueType = "branch")
            
            # init object
            private$handleBranchPopUtils <- MultifileLabelPopUtils$new(
              self$persistentObjectDirectory(),
              valueNames[!is.na(stringr::str_match(valueNames, "\\.branch$"))],
              self$imChannelNames(includeTypes = TRUE)
            )
            
            # init reactivity
            if (private$isReactive()) {
              private$handleBranchPopUtils$reactive()
            }
          }
        }
      }
      
      private$handleBranchPopUtils
    },
    
    #' @description Live population utils
    #' @param forceReload boolean to force reload data
    #' @param init boolean to init data
    livePopUtils = function(forceReload = FALSE, init = TRUE) {
      if (init == TRUE) {
        if (!is.null(self$valueNames("imLabelPropsFilepath"))) {
          if (is.null(private$handleLivePopUtils) || forceReload == TRUE) {
            # init object
            private$handleLivePopUtils <- MultifileLabelPopUtils$new(
              self$persistentObjectDirectory(),
              self$valueNames("imLabelPropsFilepath", valueType = "live"),
              self$imChannelNames(includeTypes = TRUE)
            )
            
            # init reactivity
            if (private$isReactive()) {
              private$handleLivePopUtils$reactive()
            }
          }
        }
      }
      
      private$handleLivePopUtils
    },
    
    #' @description Adata
    #' @param popType character for population type
    #' @param forceReload boolean to force reload data
    #' @param init boolean to init data
    #' @param adataPath character for filepath
    adataUtils = function(popType, forceReload = FALSE, init = TRUE, adataPath = NULL) {
      if (is.null(adataPath))
        adataPath <- self$imAnndataFilepath()
      
      # check whether utils is already set
      adataUtils <- .getVersionedVar(private$anndataUtils, valueName = popType)
      
      if (init == TRUE) {
        if (!purrr::is_empty(adataPath)) {
          if (is.null(adataUtils) || forceReload == TRUE) {
            if (file.exists(adataPath)) {
              imChannels <- self$imChannelNames(includeTypes = TRUE)
              
              # init object
              adataUtils <- AnndataUtils$new(
                adataPath, imChannels, attr(imChannels, "measure")
              )
              
              # init reactivity
              if (private$isReactive()) {
                adataUtils$reactive()
              }
              
              # set version
              private$anndataUtils <- .setVersionedVar(
                private$anndataUtils, adataUtils,
                valueName = popType)
            }
          }
        }
      }
      
      adataUtils
    },
    
    #' @description Save pop map to file
    #' @param popType character for population type
    #' @param invalidate boolean to invalidate object
    #' @param includeFiltered boolean to include filtered populations
    savePopMap = function(popType, invalidate = TRUE, includeFiltered = FALSE) {
      # get mapping from image
      popMap <- self$imPopMap(popType, includeFiltered = includeFiltered)
      
      # save as json
      exportJSON <- jsonlite::toJSON(popMap)
      
      # create pop dir
      popDir <- file.path(
        self$persistentObjectDirectory(),
        cciaConf()$dirs$tasks$populations)
      
      dir.create(popDir)
      
      # save in task directory
      write(exportJSON,
            file.path(popDir, paste0(popType, ".json"))
        )
      
      private$invalidate(invalidate = invalidate)
    },
    
    #' @description Add filtered populations
    #' @param popType character for population type
    #' @param parentPops list of character for parent populations
    #' @param pops list of character for populations
    #' @param valueName character for value name
    #' @param tracksOnly boolean to return tracks onlye
    addFilteredPops = function(popType, parentPops, pops, valueName = "default",
                               tracksOnly = FALSE) {
      popIDs <- c()
      
      # go through parents
      for (parentPop in parentPops) {
        # go through populations
        for (i in names(pops)) {
          x <- pops[[i]]
          
          # add population values
          popAttr <- list(
            parent = parentPop,
            path = paste(parentPop, i, sep = "/"),
            valueName = valueName,
            filterCategory = if ("filterCategory" %in% names(x)) x$filterCategory else NULL,
            filterValues = x$filterValues,
            filterMeasure = if ("filterMeasure" %in% names(x)) x$filterMeasure else NULL,
            filterMeasures = if ("filterMeasures" %in% names(x)) x$filterMeasures else NULL,
            filterFun = if ("filterFun" %in% names(x)) x$filterFun else NULL,
            filterFuns = if ("filterFuns" %in% names(x)) x$filterFuns else NULL,
            filterDefaultAll = if ("filterDefaultAll" %in% names(x)) x$filterDefaultAll else FALSE,
            isTrack = if ("isTrack" %in% names(x)) x$isTrack else FALSE
          )
          
          # remove null
          popAttr <- popAttr[lengths(popAttr) > 0]
          
          # add population to map
          popIDs <- c(popIDs, self$addPop(
            popType,
            popName = i,
            popAttr = popAttr,
            popColour = x$colour,
            includeFiltered = TRUE
          ))
        }
      }
      
      # save pops
      self$savePopMap(popType, includeFiltered = TRUE)
      
      popIDs
    },
    
    #' @description Add population
    #' @param popType character for population type
    #' @param popName character for population name
    #' @param popAttr list of character for population attributes
    #' @param invalidate boolean to invalidate object
    #' @param includeFiltered boolean to include filtered populations
    #' @param popColour character for population colour
    addPop = function(popType, popName = NULL, popAttr = list(), invalidate = TRUE,
                      includeFiltered = FALSE, popColour = NULL) {
      # get mapping from image
      popMap <- self$imPopMap(popType, includeFiltered = includeFiltered)
      
      # init map
      if (is.null(popMap)) {
        popMap <- list()
      }
      
      # assign random colour
      # randCols <- distinctColorPalette(
      #   cciaConf()$colours$nDistinct)
      if (is.null(popColour)) {
        popColour <- sample(cciaConf()$colours$predefined, 1)
      }
      
      # generate a unique id
      popID <- genUID(6)
      
      if (is.null(popName)) popName <- popID
      
      # add population with zero list
      if (!(popID %in% names(popMap))) {
        # popMap[[popName]] <- append(
        popMap[[popID]] <- append(
          list(
            name = popName,
            colour = popColour,
            show = TRUE
            ),
          # add further attributes
          popAttr
        )
      }
      
      # set mapping
      self$setImPopMap(popType, popMap, invalidate = invalidate)
      
      # return ID
      popID
    },
    
    #' @description Set attributes for population
    #' @param popType character for population type
    #' @param popID character for population ID
    #' @param popAttr character for population attribute
    #' @param invalidate boolean to invalidate object
    #' @param includeFiltered boolean to include filtered populations
    setPopAttr = function(popType, popID, popAttr, invalidate = TRUE, includeFiltered = FALSE) {
      # get mapping from image
      popMap <- self$imPopMap(popType, includeFiltered = includeFiltered)
      
      if (length(popID) > 0 && popID %in% names(popMap)) {
        # add attributes
        for (i in names(popAttr)) {
          x <- popAttr[[i]]
          
          popMap[[popID]][[i]] <- x
        }
      }
      
      # set mapping
      self$setImPopMap(popType, popMap, invalidate = invalidate)
    },
    
    #' @description Pop attribute
    #' @param popType character for population type
    #' @param popAttr character for population attribute
    #' @param popIDs list of character for population IDs
    #' @param popPath character for population path
    #' @param includeFiltered boolean to include filtered populations
    #' @param selectedOnly boolean to purge if none matched
    popAttr = function(popType, popAttr, popIDs = NULL, popPath = NULL,
                       includeFiltered = FALSE, selectedOnly = FALSE) {
      # get mapping from image
      popMap <- self$imPopMap(popType, includeFiltered = includeFiltered)
      
      # get popID if path is set
      if (!is.null(popPath)) {
        popIDs <- self$popIDFromPath(popMap, popPath)
      }
      
      popAttr <- lapply(popMap, function(x) if(popAttr %in% names(x)) x[[popAttr]])
      
      # get attribute for population
      if (!is.null(popIDs) || selectedOnly == TRUE) {
        popAttr <- popAttr[names(popAttr) %in% popIDs]
      }
      
      popAttr
    },
    
    #' @description Pop id from path
    #' @param popMap list of pop map
    #' @param popPath character for population path
    popIDFromPath = function(popMap, popPath) {
      popIDs <- self$popIDsFromPaths(popMap, c(popPath))
      
      if (length(popIDs) > 0) {
        return(popIDs[[1]])
      } else {
        NULL
      }
    },
    
    #' @description Pop ids from paths
    #' @param popMap list of pop map
    #' @param popPaths list of character for population paths
    popIDsFromPaths = function(popMap, popPaths) {
      popIDs <- lapply(popMap, function(x) {
        if (x$path %in% popPaths)
          unname(x$path)
        else
          NULL
      })
      
      # remove NULL
      popIDs <- popIDs[lengths(popIDs) > 0]
      
      # reverse
      if (length(popIDs) > 0)
        .reverseNamedList(popIDs)
      else
        popIDs
    },
    
    #' @description Pop leaves
    #' @param popType character for population type
    #' @param pop character for population
    #' @param direct boolean for direct population leaves
    #' @param includeFiltered boolean to include filtered population
    popLeaves = function(popType, pop, direct = FALSE, includeFiltered = FALSE) {
      leaveIDs <- c()
      
      # check if pop is root
      pop <- .flowNormRootPath(pop, defaultVal = "root")
      
      # get all populations that have this population as parent
      if (direct == TRUE) {
        leaveIDs <- self$popIDsByAttr(
          popType, "parent", pop, compareFun = "eq", includeFiltered = includeFiltered
        )
      } else {
        if (pop == "root") {
          leaveIDs <- names(self$imPopMap(popType, includeFiltered = includeFiltered))
        } else {
          leaveIDs <- self$popIDsByAttr(
            popType, "parent", pop, compareFun = "startsWith",
            includeFiltered = includeFiltered
          )
        }
      }
      
      # add paths to values
      if (length(leaveIDs) > 0) {
        # names(leaveIDs) <- self$popAttr(popType, "path", popIDs = leaveIDs)
        names(leaveIDs) <- leaveIDs
        leaveIDs <- self$popAttr(popType, "path", popIDs = leaveIDs,
                                 includeFiltered = includeFiltered)
      }
      
      leaveIDs
    },
    
    #' @description Pop IDs by attribut list
    #' @param popType character for population type
    #' @param popAttrList list of character for population attributes
    #' @param combineLogic character for combine logic
    #' @param compareFun character for compare function
    #' @param includeFiltered boolean to include filtered populations
    popIDsByAttrList = function(popType, popAttrList,
                                combineLogic = "AND", compareFun = "eq",
                                includeFiltered = FALSE) {
      listOfIDs <- list()
      popIDs <- list()
      
      # go through list and get pop ids with attributes
      for (i in names(popAttrList)) {
        x <- popAttrList[[i]]
        
        # expand channels
        if (i == "channels") {
          popIDs.x <- self$popIDsByAttr(
            popType, "xChannel", x, compareFun = "in",
            includeFiltered = includeFiltered)
          popIDs.y <- self$popIDsByAttr(
            popType, "yChannel", x, compareFun = "in",
            includeFiltered = includeFiltered)
          
          listOfIDs[[i]] <- Reduce(intersect, list(popIDs.x, popIDs.y))
        } else {
          listOfIDs[[i]] <- self$popIDsByAttr(
            popType, i, x, compareFun = compareFun,
            includeFiltered = includeFiltered)
        }
      }
      
      # combine by logic
      if (combineLogic == "AND") {
        popIDs <- Reduce(intersect, listOfIDs)
      } else if (combineLogic == "OR") {
        popIDs <- Reduce(union, listOfIDs)
      }
      
      popIDs
    },
    
    #' @description Pop IDs by attribute
    #' @param popType character for population type
    #' @param popAttrKey character of attribute key
    #' @param popAttrValue character of attribute value
    #' @param compareFun character for compare function
    #' @param includeFiltered boolean to include filtered populations
    popIDsByAttr = function(popType, popAttrKey, popAttrValue, compareFun = "eq",
                            includeFiltered = FALSE) {
      retVal <- NULL
      
      # get values from populations
      popValues <- self$popAttr(popType, popAttrKey,
                                includeFiltered = includeFiltered)
      
      # exclude NULL ?
      # popValues <- popValues[lengths(popValues) > 0]
      # convert to FALSE
      popValues[lengths(popValues) == 0] <- FALSE
      
      # filter populations for value
      if (compareFun == "eq") {
        retVal <- names(popValues)[popValues == popAttrValue]
      } else if (compareFun == "in") {
        retVal <- names(popValues)[popValues %in% popAttrValue]
      } else if (compareFun == "startsWith") {
        retVal <- names(popValues)[unlist(
          lapply(popValues, function(x) startsWith(x, popAttrValue))
          )]
      } else if (compareFun == "endsWith") {
        retVal <- names(popValues)[unlist(
          lapply(popValues, function(x) endsWith(x, popAttrValue))
          )]
      } else if (compareFun == "regexp") {
        retVal <- names(popValues)[unlist(
          lapply(popValues, function(x) stringr::str_match(x, popAttrValue))
          )]
      }
      
      retVal
    },
    
    #' @description Pop ID by attribute
    #' @param ... passed to self$popIDsByAttr
    popIDByAttr = function(...) {
      popIDs <- self$popIDsByAttr(...)
      
      # return first hit
      if (length(popIDs) > 0) {
        popIDs <- popIDs[[1]]
      }
      
      popIDs
    },
    
    #' @description Delete population
    #' @param popType character for population type
    #' @param popIDs list of character for population IDs
    #' @param popPath character for population path
    #' @param invalidate boolean to invalidate object
    #' @param includeFiltered boolean to include filtered populations
    delPop = function(popType, popIDs = NULL, popPath = NULL, invalidate = TRUE, includeFiltered = FALSE) {
      # get mapping from image
      popMap <- self$imPopMap(popType, includeFiltered = includeFiltered)
      
      # get popID if path is set
      if (!is.null(popPath)) {
        popIDs <- self$popIDFromPath(popMap, popPath)
      }
      
      # delete population from list
      if (popIDs %in% names(popMap)) {
        popMap[[popIDs]] <- NULL
      }
      
      # set mapping
      self$setImPopMap(popType, popMap, invalidate = invalidate)
    },
    
    #' @description Delete populations by path
    #' @param popType character for population type
    #' @param pops list of character for populations
    #' @param invalidate boolean to invalidate object
    #' @param includeFiltered boolean to include filtered populations
    delPopsByPath = function(popType, pops, invalidate = TRUE, includeFiltered = FALSE) {
      for (x in pops) {
        popID <- self$popIDByAttr(
          popType, "path", x, includeFiltered = includeFiltered)
        
        if (length(popID) > 0)
          self$delPop(popType, popID, includeFiltered = includeFiltered)
      }
    },
    
    #' @description Toggle visibility for pop
    #' @param popType character for population type
    #' @param popID character for population ID
    #' @param invalidate boolean to invalidate object
    #' @param includeFiltered boolean to include filtered populations
    toggleVisibilityForPop = function(popType, popID, invalidate = TRUE,
                                      includeFiltered = FALSE) {
      # get mapping from image
      popMap <- self$imPopMap(popType, includeFiltered = includeFiltered)
      
      # delete population from list
      if (popID %in% names(popMap)) {
        popMap[[popID]]$show <- if (popMap[[popID]]$show == TRUE)
           FALSE else TRUE
      }
      
      # set mapping
      self$setImPopMap(popType, popMap, invalidate = invalidate)
    },
    
    #' @description Rename population
    #' @param popType character for population type
    #' @param popID character for population ID
    #' @param newPopName character for new population name
    #' @param invalidate boolean to invalidate object
    #' @param includeFiltered boolean to include filtered populations
    editPopName = function(popType, popID, newPopName, invalidate = TRUE,
                           includeFiltered = FALSE) {
      # get mapping from image
      popMap <- self$imPopMap(popType, includeFiltered = includeFiltered)
      
      # rename population
      if (popID %in% names(popMap)) {
        popMap[[popID]]$name <- newPopName
      }
      
      # set mapping
      self$setImPopMap(popType, popMap, invalidate = invalidate)
    },
    
    #' @description Set colour for population
    #' @param popType character for population type
    #' @param popID character for population ID
    #' @param colour character for population colour
    #' @param invalidate boolean to invalidate object
    #' @param includeFiltered boolean to include filtered populations
    editPopColour = function(popType, popID, colour, invalidate = TRUE,
                             includeFiltered = FALSE) {
      # get mapping from image
      popMap <- self$imPopMap(popType, includeFiltered = includeFiltered)
      
      # set colour  
      if (popID %in% names(popMap)) {
        popMap[[popID]]$colour <- colour
      }
      
      # set mapping
      self$setImPopMap(popType, popMap, invalidate = invalidate)
    },
    
    #' @description Save data
    saveData = function() {
      # go through available population utils
      for (i in c("flow", "live")) {
        if (!is.null(self$popUtils(i, init = FALSE))) {
          self$popUtils(i)$save()
          
          # # save population to labels
          # TODO do I need this ..?
          # if (!is.null(self$labelProps())) {
          #   self$savePopsToLabels(i)
          # }
        }
      }
    },
    
    #' @description Save populations to labels
    #' @param popType character of population type
    savePopsToLabels = function(popType) {
      # get populations
      popLabels <- self$popUtils(popType)$popLabels()
      popCol <- as.list(popLabels[, c("pop")])
      names(popCol) <- c(paste(popType, "pop", sep = "."))
      
      # add to labels
      labels <- self$labelProps()
      labels$add_obs(popCol)
      
      # save
      labels$save()
      labels$close()
    },
    
    #' @description Reset loaded data
    resetData = function() {
      private$anndataUtils <- NULL
      private$labelPropsUtils <- NULL
      private$handleFlowGatingSet <- NULL
    },
    
    #' @description Load data
    #' @param forceReload boolean to force reload data
    loadData = function(forceReload = TRUE) {
      # reload anndata
      # self$adataUtils(forceReload = forceReload)
      
      # reload label props
      # self$labelProps(forceReload = forceReload)
      
      # reload gatingset
      # self$flowGatingSet(forceReload = forceReload)
    },
    
    #' @description Values for versioned variable
    #' @param x character for variable to get from self$getCciaMeta()
    values = function(x) {
      objMeta <- self$getCciaMeta()
      retVal <- NULL
      
      # check if value is present
      if (x %in% names(objMeta)) {
        return(objMeta[[x]])
      }
      
      retVal
    },
    
    #' @description Value default for versioned variable
    #' @param x character for variable to get from self$getCciaMeta()
    #' @param default boolean to use default value
    valueDefault = function(x, default = FALSE) {
      objMeta <- self$getCciaMeta()
      retVal <- NULL
      
      # check if value is present
      if (x %in% names(objMeta)) {
        # add attributes
        retVal <- attr(objMeta[[x]], "default")
      }
      
      retVal
    },
    
    #' @description Value name for popType
    #' @param popType character for population type
    #' @param ... passed to self$valueNames
    valueNamesForPopType = function(popType, ...) {
      if (popType == "flow") {
        return(self$valueNames("imGatingSetFilepath", ...))
      } else if (popType == "clust") {
        return(self$valueNames("imAnndataFilepath", ...))
      } else if (popType == "region") {
        return(self$valueNames("imRegionsFilepath", ...))
      } else if (popType == "live") {
        return(self$valueNames("imLabelPropsFilepath", valueType = "live", ...))
      } else if (popType == "clsf") {
        return(self$valueNames("imLabelPropsFilepath", valueType = "clsf", ...))
      } else if (popType == "branch") {
        return(self$valueNames("imLabelPropsFilepath", valueType = "branch", ...))
      } else if (popType == "labels") {
        return(self$valueNames("imLabelPropsFilepath", valueType = "labels", ...))
      }
    },
    
    #' @description Value names for versioned variable
    #' @param x character for variable to get from self$getCciaMeta()
    #' @param valueType character for value type
    #' @param defaultOnly boolean to return default only
    valueNames = function(x, valueType = NULL, defaultOnly = FALSE) {
      objMeta <- self$getCciaMeta()
      retVal <- NULL
      
      # check if value is present
      if (x %in% names(objMeta)) {
        retVal <- as.list(names(objMeta[[x]]))
        
        # add attributes
        attr(retVal, "default") <- attr(objMeta[[x]], "default")
      }
      
      # add names
      names(retVal) <- retVal
      
      # only return a specific value type?
      if (!is.null(valueType)) {
        strReg <- NULL
        
        if (valueType == "clsf")
          strReg <- "cl"
        else if (valueType == "branch")
          strReg <- "branch"
        
        # TODO this should be better
        if (!is.null(strReg))
          retVal <- retVal[!is.na(stringr::str_match(retVal, paste0("\\.", strReg,"$")))]
        else
          retVal <- retVal[is.na(stringr::str_match(retVal, paste0("\\.", strReg,"$")))]
      }
      
      # return only default?
      if (defaultOnly == TRUE)
        retVal <- attr(retVal, "default")
      
      retVal
    },
    
    #' @description Value part of for versioned variable
    #' @param x character of variable
    #' @param valueName character of value name
    valuePartOf = function(x, valueName = "default") {
      self$valueAttr(x, "partOf", valueName = valueName)
    },
    
    #' @description Value suffixes for versioned variable
    #' @param x character of variable
    #' @param valueName character of value name
    valueSuffixes = function(x, valueName = "default") {
      self$valueAttr(x, "suffixes", valueName = valueName)
    },
    
    #' @description Value locations for versioned variable
    #' @param x character of variable
    #' @param valueName character of value name
    valueLocUID = function(x, valueName = "default") {
      self$valueAttr(x, "locUID", valueName = valueName)
    },
    
    #' @description Value attr for versioned variable
    #' @param x character of variable
    #' @param valueName character of value name
    valueAttr = function(x, attrName, valueName = "default") {
      objMeta <- self$getCciaMeta()
      retVal <- NULL
      
      # check if value is present
      if (x %in% names(objMeta)) {
        # get attributes
        retVal <- attr(objMeta[[x]][[valueName]], attrName)
      }
      
      retVal
    },
    
    ## setters
    #' @description Set default for value names
    #' @param x character for variable to get from self$getCciaMeta()
    #' @param valueName character to set as default
    #' @param invalidate boolean to invalidate object
    setValueNameDefault = function(x, valueNameDefault, invalidate = TRUE) {
      objMeta <- self$getCciaMeta()
      retVal <- NULL
      
      # check if value is present
      if (x %in% names(objMeta) && valueNameDefault %in% names(objMeta[[x]])) {
        attr(objMeta[[x]], "default") <- valueNameDefault
        
        self$setCciaMeta(objMeta, invalidate = invalidate)
      }
    },
    
    setOriFilepath = function(x, invalidate = TRUE) {
      objMeta <- self$getCciaMeta()
      objMeta$oriFilepath <- x
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImFilepath = function(x, valueName = NULL, setDefault = TRUE,
                             invalidate = TRUE, reset = FALSE) {
      objMeta <- self$getCciaMeta()
      
      objMeta <- .setVersionedVarInList(
        objMeta, "imFilepath", x, valueName = valueName,
        setDefault = setDefault)
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImMeta = function(x, invalidate = TRUE) {
      objMeta <- self$getCciaMeta()
      objMeta$imMeta <- x
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImChannelNames = function(x, valueName = NULL, setDefault = TRUE,
                                 invalidate = TRUE, reset = FALSE,
                                 addNames = TRUE, checkLength = TRUE,
                                 updateFlowGatingSet = FALSE) {
      objMeta <- self$getCciaMeta()
      
      lengthOk <- TRUE
      
      # check length versus channel size
      if (checkLength == TRUE && self$omeXMLPixels()$SizeC != length(x))
        lengthOk <- FALSE
          
      if (lengthOk == TRUE) {
        # add names to channels
        if (addNames == TRUE) {
          names(x) <- paste0(
            "Chn", seq(as.integer(length(x))))
        }
        
        objMeta <- .setVersionedVarInList(
          objMeta, "imChannelNames", x, valueName = valueName,
          setDefault = setDefault, reset = reset)
        
        self$setCciaMeta(objMeta, invalidate = invalidate)
        
        # update flow gating set
        if (updateFlowGatingSet == TRUE) {
          fgs <- self$flowGatingSet()
          
          if (!is.null(fgs)) {
            # fgs$renameColumns(unname(x))
            fgs$renameColumns(.flowCorrectChannelNames(unname(x)))
          }
        }
      }
    },
    
    setImLabelsFilepath = function(x, valueName = NULL, setDefault = TRUE,
                                   invalidate = TRUE, reset = FALSE) {
      objMeta <- self$getCciaMeta()
      
      objMeta <- .setVersionedVarInList(
        objMeta, "imLabelsFilepath", x, valueName = valueName,
        setDefault = setDefault, reset = reset)
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImLabelPropsFilepath = function(x, valueName = NULL, setDefault = TRUE,
                                        invalidate = TRUE, reset = FALSE) {
      objMeta <- self$getCciaMeta()
      
      objMeta <- .setVersionedVarInList(
        objMeta, "imLabelPropsFilepath", x, valueName = valueName,
        setDefault = setDefault, reset = reset)
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImRegionsFilepath = function(x, valueName = NULL, setDefault = TRUE,
                                    invalidate = TRUE, reset = FALSE) {
      objMeta <- self$getCciaMeta()
      
      objMeta <- .setVersionedVarInList(
        objMeta, "imRegionsFilepath", x, valueName = valueName,
        setDefault = setDefault, reset = reset)
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImNeighboursFilepath = function(x, valueName = NULL, setDefault = TRUE,
                                    invalidate = TRUE, reset = FALSE) {
      objMeta <- self$getCciaMeta()
      
      objMeta <- .setVersionedVarInList(
        objMeta, "imNeighboursFilepath", x, valueName = valueName,
        setDefault = setDefault, reset = reset)
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImPopMap = function(popType, x, mergeMap = FALSE, invalidate = TRUE) {
      objMeta <- self$getCciaMeta()
      
      # merge with existing map
      # this happens when the map was filtered in classifier
      # or for clustering and then saved back
      if (mergeMap == TRUE) {
        for (i in names(x)) {
          objMeta$imPopMap[[popType]][[i]] <- x[[i]]
        }
      } else {
        objMeta$imPopMap[[popType]] <- x
      }
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImPopGatePlot = function(popType, plotID = NULL, plotParams = NULL,
                                invalidate = TRUE) {
      objMeta <- self$getCciaMeta()
      
      # add type
      if (!popType %in% names(objMeta$imPopGatePlots)) {
        objMeta$imPopGatePlots[[popType]] <- list()
      }
      
      # add params
      if (!is.null(plotID)) {
        if (plotID > length(objMeta$imPopGatePlots[[popType]])) {
          objMeta$imPopGatePlots[[popType]][[plotID]] <- list()
        }
        
        for (i in names(plotParams)) {
          x <- plotParams[[i]]
          
          objMeta$imPopGatePlots[[popType]][[plotID]][[i]] <- x
        }
      } else {
        objMeta$imPopGatePlots[[popType]] <- plotParams
      }
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImGatingSetFilepath = function(x, valueName = NULL, setDefault = TRUE,
                                      invalidate = TRUE, reset = FALSE) {
      objMeta <- self$getCciaMeta()
      
      objMeta <- .setVersionedVarInList(
        objMeta, "imGatingSetFilepath", x, valueName = valueName,
        setDefault = setDefault, reset = reset)
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    setImAnndataFilepath = function(x, valueName = NULL, setDefault = TRUE,
                                    invalidate = TRUE, reset = FALSE) {
      objMeta <- self$getCciaMeta()
      
      objMeta <- .setVersionedVarInList(
        objMeta, "imAnndataFilepath", x, valueName = valueName,
        setDefault = setDefault, reset = reset)
      
      self$setCciaMeta(objMeta, invalidate = invalidate)
    },
    
    ## getters
    imMeta = function() {
      retVal = NULL
      
      if ("imMeta" %in% names(self$getCciaMeta())) {
        retVal <- self$getCciaMeta()$imMeta
      }
      
      retVal
    },
    
    imChannelNames = function(valueName = NULL, useNames = TRUE,
                              correctChannelNames = FALSE, includeTypes = FALSE,
                              rmAttr = FALSE, includeMeasure = TRUE) {
      channelNames <- .getVersionedVarInList(
        self$getCciaMeta(), "imChannelNames", valueName = valueName)
      
      # include types of channels?
      if (includeTypes == TRUE) {
        # add further channel types
        if (!is.null(attr(channelNames, "types"))) {
          # retain attributes
          channelAttr <- attributes(channelNames)
          
          channelNames <- c(
            channelNames,
            unlist(lapply(
              attr(channelNames, "types"),
              function(x) {
                y <- paste(x, channelNames, sep = "_")
                # set names
                names(y) <- sprintf(
                  "%s_Chn%s", x, seq(length(channelNames)))
                y
              }
            ))
          )
          
          # TODO is there a better way?  
          # attributes(channelNames) <- channelAttr[names(channelAttr) != "names"]
          for (i in names(channelAttr)[names(channelAttr) != "names"]) {
            attr(channelNames, i) <- channelAttr[[i]]
          }
        }
      }
      
      if (!is.null(channelNames)) {
        # include measures?
        if (includeMeasure == TRUE) {
          # default to mean
          if (is.null(attr(channelNames, "measure"))) {
            attr(channelNames, "measure") <- "mean"
          }
        }
        
        # remove names?
        if (useNames == FALSE) {
          names(channelNames) <- NULL
        }
        
        # correct channel names?
        if (correctChannelNames == TRUE) {
          channelNames <- .flowCorrectChannelNames(channelNames)
        }
        
        # remove attributes?
        if (rmAttr == TRUE) {
          attributes(channelNames) <- NULL
        }
      }
      
      channelNames
    },
    
    # imChannelsMeasure = function() {
    #   channelNames <- .getVersionedVarInList(
    #     self$getCciaMeta(), "imChannelNames", valueName = valueName)
    #   
    #   measure <- attr(channelNames, "measure")
    #   
    #   # default to mean
    #   if (is.null(measure)) {
    #     measure <- "mean"
    #   }
    #   
    #   measure
    # },
    
    oriFilepath = function(modified = FALSE, revertToOri = FALSE) {
      retVal = NULL
      
      if ("oriFilepath" %in% names(self$getCciaMeta())) {
        retVal <- self$getCciaMeta()$oriFilepath
        
        # get modified version?
        if (modified == TRUE) {
          # this will return NULL if none found
          modFilepath <- attr(retVal, "modified")
          
          # revert to original?
          if (is.null(modFilepath)) {
            if (revertToOri == FALSE)
              retVal <- NULL
            else
              attr(retVal, "modified") <- FALSE
          } else {
            retVal <- modFilepath
            
            attr(retVal, "modified") <- TRUE
          }
        }
      }
      
      retVal
    },
    
    imFilepath = function(valueName = NULL, absolutePath = TRUE) {
      retVal <- .getVersionedVarInList(
        self$getCciaMeta(), "imFilepath", valueName = valueName)
      
      # get absolute path?
      if (!is.null(retVal)) {
        if (absolutePath == TRUE) {
          retVal <- self$persistentObjectDirectoryFile(
            retVal, zero = TRUE)
        }
      }
      
      retVal
    },
    
    imLabelsFilepath = function(valueName = NULL, absolutePath = TRUE) {
      retVal <- .getVersionedVarInList(
        self$getCciaMeta(), "imLabelsFilepath", valueName = valueName)
      
      # add task directory
      retVal <- file.path(cciaConf()$dirs$tasks$labels, retVal)
      
      # get absolute path?
      if (!is.null(retVal)) {
        if (absolutePath == TRUE) {
          retVal <- self$persistentObjectDirectoryFile(retVal)
        }
      }
      retVal
    },
    
    imRegionsFilepath = function(valueName = NULL, absolutePath = TRUE) {
      retVal <- .getVersionedVarInList(
        self$getCciaMeta(), "imRegionsFilepath", valueName = valueName)
      
      # add task directory
      retVal <- file.path(cciaConf()$dirs$tasks$labelProps, retVal)
      
      # get absolute path?
      if (!is.null(retVal)) {
        if (absolutePath == TRUE) {
          retVal <- self$persistentObjectDirectoryFile(retVal)
        }
      }
      
      retVal
    },
    
    imNeighboursFilepath = function(valueName = NULL, absolutePath = TRUE) {
      retVal <- .getVersionedVarInList(
        self$getCciaMeta(), "imNeighboursFilepath", valueName = valueName)
      
      # add task directory
      retVal <- file.path(cciaConf()$dirs$tasks$labelProps, retVal)
      
      # get absolute path?
      if (!is.null(retVal)) {
        if (absolutePath == TRUE) {
          retVal <- self$persistentObjectDirectoryFile(retVal)
        }
      }
      
      retVal
    },
    
    imLabelPropsFilepath = function(valueName = NULL, absolutePath = TRUE) {
      retVal <- .getVersionedVarInList(
        self$getCciaMeta(), "imLabelPropsFilepath", valueName = valueName)
      
      # add task directory
      retVal <- file.path(cciaConf()$dirs$tasks$labelProps, retVal)
      
      # get absolute path?
      if (!is.null(retVal)) {
        if (absolutePath == TRUE) {
          retVal <- self$persistentObjectDirectoryFile(retVal)
        }
      }
      
      retVal
    },
    
    imGatingSetFilepath = function(valueName = NULL, absolutePath = TRUE) {
      retVal = NULL
      
      retVal <- .getVersionedVarInList(
        self$getCciaMeta(), "imGatingSetFilepath", valueName = valueName)
      
      # load from different image?
      # ie/ from image set if part of a combined gating set
      gsLocUID <- self$valueLocUID("imGatingSetFilepath")
      
      # add task directory
      retVal <- file.path(cciaConf()$dirs$tasks$data, retVal)
      
      # get absolute path?
      if (!is.null(retVal)) {
        if (absolutePath == TRUE) {
          if (!is.null(gsLocUID))
            retVal <- self$persistentObjectDirectoryFile(retVal, uID = gsLocUID)
          else
            retVal <- self$persistentObjectDirectoryFile(retVal)
        }
      }
      
      retVal
    },
    
    imAnndataFilepath = function(valueName = NULL, absolutePath = TRUE) {
      retVal = NULL
      
      retVal <- .getVersionedVarInList(
        self$getCciaMeta(), "imAnndataFilepath", valueName = valueName)
      
      savedIn <- NULL
      if ("savedIn" %in% names(attributes(retVal)))
        savedIn <- attr(retVal, "savedIn")
      
      # add task directory
      retVal <- file.path(cciaConf()$dirs$tasks$labelProps, retVal)
      
      # get absolute path?
      if (!is.null(retVal)) {
        if (absolutePath == TRUE) {
          if (!is.null(savedIn)) {
            retVal <- file.path(
              dirname(self$persistentObjectDirectory()), savedIn, retVal)
            attr(retVal, "savedIn") <- savedIn
          } else {
            retVal <- self$persistentObjectDirectoryFile(retVal)
          }
        }
      }
      
      retVal
    },
    
    imShapes = function(shapeType = "region", valueName = "default") {
      # TODO this should be done in a more reactive way
      # At the moment, napari is in charge of saving shapes
      # which is why shiny has to get the current
      # shape files from disk
      shapeFiles <- list.files(
        file.path(self$persistentObjectDirectory(),
                  cciaConf()$dirs$tasks$shapes,
                  valueName),
        pattern = "^.*.csv$")
      
      # return names of shapes
      tools::file_path_sans_ext(shapeFiles)
    },
    
    imPopMap = function(popType = NULL, popIDs = NULL, popPath = NULL,
                        tracksOnly = FALSE, includeFiltered = FALSE,
                        filterMeasures = NULL) {
      retVal = NULL
      
      if ("imPopMap" %in% names(self$getCciaMeta())) {
        popMap <- self$getCciaMeta()$imPopMap
        
        # return map of type
        if (!is.null(popType)) {
          if (popType %in% names(popMap)) {
            retVal <- popMap[[popType]]
          }
          
          if (!is.null(retVal)) {
            # get popID if path is set
            if (!is.null(popPath)) {
              popIDs <- sapply(retVal, function(x) {
                if (x$path == popPath)
                  TRUE
                else
                  NULL
              })
              popIDs <- names(popIDs[lengths(popIDs) > 0])
            }
          
            # return info for selected IDs
            if (!is.null(popIDs)) {
              retVal <- retVal[names(retVal) %in% popIDs]
            } else {
              # include filtered?
              if (length(retVal) > 0) {
                if (includeFiltered == FALSE) {
                  retVal <- retVal[
                    sapply(retVal, function(x) !any(c("filterMeasure", "filterMeasures") %in% names(x)))
                  ]
                } else {
                  # include only certain filtered measures?
                  if (length(filterMeasures) > 0) {
                    retVal <- retVal[
                      sapply(retVal, function(x) {
                        if ("filterMeasure" %in% names(x) && !x$filterMeasure %in% filterMeasures)
                          FALSE
                        # TODO cleaner?
                        else if ("filterMeasures" %in% names(x) && !all(x$filterMeasures %in% filterMeasures))
                          FALSE
                        else
                          TRUE
                      }) 
                    ]
                  }
                }
              }
            }
          }
        } else {
          retVal <- popMap
        }
      }
      
      # filter for cells and tracks
      if (tracksOnly == TRUE) {
        retVal <- lapply(retVal, function(x) {
          if ("isTrack" %in% names(x)) {
            if (x$isTrack == TRUE) {
              return(x)
            } else {
              return(NULL)
            }
          } else {
            return(NULL)
          }
        })
      }
      
      retVal[lengths(retVal) > 0]
    },
    
    imPopGatePlots = function(popType = NULL, plotID = NULL) {
      retVal = NULL
      
      if ("imPopGatePlots" %in% names(self$getCciaMeta())) {
        gatePlots <- self$getCciaMeta()$imPopGatePlots
        
        # return gates of type
        if (!is.null(popType)) {
          if (popType %in% names(gatePlots)) {
            retVal <- gatePlots[[popType]]
          }
        
          # return plot with ID
          if (!is.null(plotID)) {
            if (plotID <= length(retVal)) {
              retVal <- retVal[[plotID]]
            } else {
              retVal <- NULL
            }
          }
        } else {
          retVal <- gatePlots
        }
      }
      
      retVal
    }
  )
)
