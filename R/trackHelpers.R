#' @description Filter tracks that have less than y timepoints
#' @param x celltrackR::tracks
#' @param min.timepoints integer for minimum timepoints
#' @examples
#' TODO
#' @export
tracks.fun.time.filter <- function(x, min.timepoints = 5) {
  nrow(x) >= min.timepoints
}

#' @description Combine tracks DT
#' @param x celltrackR::tracks
#' @param idcol character for ID column via data.table::rbindlist
#' @examples
#' TODO
#' @export
tracks.combine.dt <- function(x, idcol = "cell_type") {
  data.table::rbindlist(x, idcol = idcol, fill = TRUE)
}

#' @description Apply function to tracks to change values
#' @param tracks list of celltrackR::tracks
#' @param call.FUN function to call for tracks
#' @param as.dt boolean to create data.table from result
#' @param idcol character for ID column via data.table::rbindlist
#' @param ... passed to lapply
#' @examples
#' TODO
#' @export
tracks.apply.fun <- function(tracks, call.FUN, as.dt = TRUE,
                             idcol = "uID", ...) {
  tracks.fun.result <- lapply(tracks, call.FUN, ...)
  
  if (as.dt == FALSE) {
    return(tracks.fun.result)
  } else {
    # list of lists
    tracks.fun.DT <- lapply(
      tracks.fun.result,
      function(x) {
        data.table::rbindlist(lapply(
          x,
          function(y) {
            data.table::as.data.table(as.matrix(y))
          }
        ), idcol = "track_id")
      }
    )
    
    return(data.table::rbindlist(tracks.fun.DT, idcol = idcol))
  }
}

#' @description Create data frame with aggregate function
#' @param tracks list of celltrackR::tracks
#' @param call.FUN function to call for tracks
#' @param summary.FUN function passed to aggregate
#' @param result.name character for result column
#' @param add.time.delta boolean to add time delta
#' @param as.dt boolean to create data.table from result
#' @param group.tracks.by character to group tracks by
#' @param idcol character for ID column via data.table::rbindlist
#' @param subtracks.i integer to use subtracks
#' @param subtracks.overlap integer to subtrack overlap
#' @param ... passed to aggregate
#' @examples
#' TODO
#' @export
tracks.aggregate.fun <- function(tracks, call.FUN, summary.FUN,
                                 result.name = "measure",
                                 add.time.delta = FALSE,
                                 as.dt = TRUE,
                                 group.tracks.by = NULL,
                                 idcol = "uID", subtracks.i = 0,
                                 subtracks.overlap = subtracks.i - 1,
                                 ...) {
  # group tracks if given
  if (!is.null(group.tracks.by) && length(group.tracks.by) > 0) {
    tracks.groups <- list()
    
    # filter grouping for the tracks present
    group.tracks.by <- group.tracks.by[names(group.tracks.by) %in% names(tracks)]
    
    # go through filter values and create tracks
    for (x in unique(group.tracks.by)) {
      # get grouped tracks
      uIDNames <- sapply(tracks[group.tracks.by == x], is.null)
      uIDNames <- names(uIDNames[uIDNames == FALSE])
      
      # tracks.groups[[x]] <- list()
      
      if (length(uIDNames) > 0) {
        tracks.groups[[x]] <- celltrackR::as.tracks(
          unlist(tracks[uIDNames], recursive = FALSE)
        )
      }
    }
    
    # copy back
    tracks <- tracks.groups
  }
  
  # create subtracks if given
  if (subtracks.i > 0) {
    tracks <- lapply(
      tracks,
      function(x) celltrackR::subtracks(x, subtracks.i, subtracks.overlap)
      )
  }
  
  # apply function
  tracks.fun.result <- lapply(
    tracks,
    function(x) {
      if (length(x) > 0)
        aggregate(x, call.FUN, FUN = summary.FUN, ...)
      }
  )
  
  # exclude NULL
  tracks.fun.result <- tracks.fun.result[lengths(tracks.fun.result) > 0]
  
  if (as.dt == FALSE) {
    return(tracks.fun.result)
  } else {
    # convert to DT
    # https://stackoverflow.com/a/20546621/13766165
    tracks.fun.DT <- mapply(
      function(x, i) {
        if (length(i) > 0) {
          DT <- as.data.table(as.matrix(x))
          
          # add extra information
          if (add.time.delta == TRUE) {
            DT$dt <- DT$i * celltrackR::timeStep(i)
          }
          
          DT
        }
      }, 
      tracks.fun.result,
      tracks,
      SIMPLIFY = FALSE
      )
    
    # remove empty groups
    tracks.fun.DT <- tracks.fun.DT[lengths(tracks.fun.DT) > 0]
    
    # bind together with ID
    return(data.table::rbindlist(tracks.fun.DT, idcol = idcol))
  }
}

#' @description Bundle calculations results from function call
#' @param tracks list of celltrackR::tracks
#' @param call.FUN function to call for tracks
#' @param idcol character for ID column via data.table::rbindlist
#' @examples
#' TODO
#' @export
tracks.calc.fun <- function(tracks, call.FUN, idcol = "uID") {
  # apply function
  data.table::rbindlist(lapply(tracks, call.FUN), idcol = idcol)
}

#' @description Create data frame with function
#' @param tracks list of celltrackR::tracks
#' @param call.FUN function to call for tracks
#' @param result.name character for result column
#' @param as.dt boolean to create data.table from result
#' @param group.tracks.by character to group tracks by
#' @param steps.subtracks integer to use subtracks
#' @param steps.overlap integer to subtrack overlap
#' @param idcol character for ID column via data.table::rbindlist
#' @param as.degrees boolean to convert angle to degrees
#' @param ... passed to sapply
#' @examples
#' TODO
#' @export
tracks.measure.fun <- function(tracks, call.FUN, result.name = "measure",
                               as.dt = TRUE, steps.subtracks = NULL,
                               steps.overlap = steps.subtracks - 1, idcol = "uID",
                               as.degrees = TRUE, ...) {
  # apply function
  tracks.fun.result <- lapply(
    tracks,
    function(x) {
      xRes <- NULL
      
      # TODO too convoluted
      # apply subtracks?
      if (!is.null(steps.subtracks)) {
        xRes <- sapply(celltrackR::subtracks(
          x, i = steps.subtracks, overlap = steps.overlap), call.FUN, simplify = FALSE, ...)
      } else {
        xRes <- sapply(x, call.FUN, simplify = FALSE, ...)
      }
      
      xRes <- do.call(rbind, xRes)
      # rownames(xRes) <- names(x)
      
      xRes
    } 
  )
  
  if (as.dt == FALSE) {
    return(tracks.fun.result)
  } else {
    DT <- NULL
    
    if (any(lengths(tracks) > 0)) {
      # split track IDs?
      # splitTrackIDs <- !is.null(steps.subtracks)
      # TODO not very elegant though
      splitTrackIDs <- stringr::str_detect(rownames(tracks.fun.result[[1]])[[1]], "[0-9]+\\.[0-9]+")
      splitTrackIDs <- if (length(splitTrackIDs) > 0) splitTrackIDs else FALSE
      
      # convert to DT
      tracks.fun.DT <- lapply(
        tracks.fun.result[lengths(tracks.fun.result) > 0],
        function(x) {
          DT <- as.data.table(as.matrix(x))[
            # , track_id := as.numeric(names(x))]
            # , track_id := if (splitTrackIDs) names(x) else as.numeric(names(x))]
            , track_id := if (splitTrackIDs) rownames(x) else as.numeric(rownames(x))]
          # ] %>% data.table::rename(!!result.name := "V1")
          
          if ("V1" %in% colnames(DT))
            setnames(DT, "V1", result.name)
          
          if (nrow(DT) > 0) DT else NULL
        }
      )
      
      # bind together with ID
      DT <- data.table::rbindlist(tracks.fun.DT, idcol = idcol, fill = TRUE)
      
      # split track id?
      if (splitTrackIDs) {
        DT[, c("track_id", "cell_id") := lapply(
          data.table::tstrsplit(track_id, ".", fixed = TRUE),
          as.numeric
        )]
        
        # TODO why do you need this?
        # increase cell ID by number of steps for subtracks
        # a cell at t0 has no speed
        # a cell at t1 has no angle
        # DT[, cell_id := cell_id + steps.subtracks]
      }
      
      # convert to degrees
      if (as.degrees == TRUE) {
        if (any(c("overallAngle", "meanTurningAngle") %in% colnames(DT))) {
          measure.x <- colnames(DT)[colnames(DT) %in% c("overallAngle", "meanTurningAngle")]
          
          DT[, (measure.x) := pracma::rad2deg(get(measure.x))]
        }
      }
    }
    
    DT
  }
}

#' @description Get start and end positions
#' @param track of celltrackR::tracks
#' @examples
#' TODO
#' @export
tracks.coords <- function(track) {
  # TODO this is very convoluted
  posHead <- head(track, 1)
  posTail <- tail(track, 1)
  
  colnames(posHead) <- paste0("start_", colnames(posHead))
  colnames(posTail) <- paste0("end_", colnames(posTail))
  
  cbind(posHead, posTail)
} 
