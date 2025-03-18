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
                               as.degrees = TRUE, increment.cell.id = FALSE, ...) {
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
        if (increment.cell.id == TRUE)
          DT[, cell_id := cell_id + steps.subtracks]
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

## Functions to modify tracks
#' @description delete points
#' @param data.table populations
#' @param integer vector of label IDs
#' @examples
#' TODO
#' @export
tracks.points.rm <- function(popDT, labelIDs) {
  # should work in place
  popDT[label %in% labelIDs, track_id := NA]
}

#' @description add points to track
#' @param data.table populations
#' @param integer vector of label IDs
#' @param integer for track ID
#' @examples
#' TODO
#' @export
tracks.points.add <- function(popDT, labelIDs, trackID = NULL) {
  # get highest track number
  if (length(trackID) <= 0)
    trackID <- max(popDT$track_id, na.rm = TRUE) + 1
  
  popDT[label %in% labelIDs, track_id := trackID]
}

#' @description delete track
#' @param data.table populations
#' @param integer vector of label IDs
#' @examples
#' TODO
#' @export
tracks.rm <- function(popDT, trackIDs) {
  # should work in place
  # popDT[track_id == trackID, track_id := NA]
  popDT[track_id %in% trackIDs, track_id := NA]
}

#' @description join tracks
#' @param data.table populations
#' @param integer track A
#' @param integer track B
#' @examples
#' TODO
#' @export
tracks.join <- function(popDT, trackID.A, trackID.B) {
  # get last point of first track
  t.a <- popDT[track_id == trackID.A]$centroid_t
  t.b <- popDT[track_id == trackID.B]$centroid_t
  # t.a.last <- t.a[length(t.a)] 
  
  # get first element of B that is not in A
  t.intersect <- intersect(t.a, t.b)
  t.b.unique <- t.b[!t.b %in% t.intersect]
  
  # now merge track points in place
  popDT[track_id == trackID.B & centroid_t %in% t.b.unique, track_id := trackID.A]
}

#' @description save modified tracks file
#' TODO this is really only for viewing in napari
#' @param cciaObject
#' @param data.table populations
#' @param character for value name
#' @param character extension name
#' @examples
#' TODO
#' @export
tracks.save.mod <- function(cciaObj, popDT, valueName, ext.mod = "-mod") {
  # get labels
  labels <- cciaObj$labelProps(valueName = valueName)
  
  if (length(labels) > 0) {
    labels.path <- labels$adata_filepath()
    
    labels.new.base <- stringr::str_replace(basename(labels.path), ".h5ad", paste0(ext.mod, ".h5ad"))
    labels.new.path <- file.path(dirname(labels.path), labels.new.base)
    
    # add to labels
    labels$add_obs(as.list(popDT[, .(track_id)]))
    
    # save
    labels$save(filename = labels.new.path)
    labels$close()
  }
}

# # save modified tracks file
# # TODO this is really only for viewing in napari
# tracks.save.mod.tracks <- function(cciaObj, trackIDs, valueName, ext.mod = "-mod") {
#   # get labels
#   labels <- cciaObj$labelProps(valueName = valueName)
#   
#   if (length(labels) > 0) {
#     labels.path <- labels$adata_filepath()
#     
#     labels.new.base <- stringr::str_replace(basename(labels.path), ".h5ad", paste0(ext.mod, ".h5ad"))
#     labels.new.path <- file.path(dirname(labels.path), labels.new.base)
#     
#     # add to labels
#     # labels$add_obs(as.list(popDT[, .(track_id)]))
#     # the other values will be removed upon save
#     # labels$view_cols(list("track_id"))
#     trackDT <- as.data.table(labels$values_obs())
#     trackDT[!track_id %in% trackIDs, track_id := NaN]
#     labels$add_obs(as.list(trackDT[, .(track_id)]))
#     
#     # save
#     labels$save(filename = labels.new.path)
#     labels$close()
#   }
# }

#' @description get track position
#' @param cciaObject
#' @param data.table populations
#' @param character vector for track IDs
#' @param numeric for pixel resolution
#' @examples
#' TODO
#' @export
tracks.pos <- function(popDT, tracksIDs, pixRes = 1) {
  # get coordinates for tracks to centre camera
  centroid.cols <- c("centroid_t", "centroid_y", "centroid_x")
  
  # https://stackoverflow.com/a/43834005
  tracks.centroids <- unlist(
    popDT[track_id %in% tracksIDs,
          sapply(.SD, function(x) list(median = round(median(x)))),
          .SDcols = centroid.cols])
  
  # set resolution for coordinates
  tracks.centroids[2:length(tracks.centroids)] <- tracks.centroids[2:length(tracks.centroids)] * pixRes
  
  names(tracks.centroids) <- centroid.cols
  tracks.centroids
}

#' @description get difference between track IDs for history
#' @param character vector for track IDs A
#' @param character vector for track IDs B
#' @examples
#' TODO
#' @export
track.diffs <- function(a, b, short = "") {
  # get differences between lists
  # TODO doesn't work for NA; probably would need to time which solution is better
  # https://stackoverflow.com/a/78724307
  # `%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
  # listDiff <- a != b
  # listDiff <- a %!=na% b
  # https://stackoverflow.com/a/61248968
  # list.diff <- (a == b) | (is.na(a) & is.na(b))
  # list.diff[is.na(list.diff)] <- FALSE
  list.diff <- paste(a) != paste(b)
  
  # a = previous values, b = current values, i = index
  list(a = a[list.diff], b = b[list.diff], i = which(list.diff), short = short)
}

#' @description roll back changes
#' @param character vector for track IDs
#' @param list of track changes
#' @param integer for position in edit history
#' @examples
#' TODO
#' @export
track.edits.rollback <- function(x, edit.history, i) {
  # get changes in reverse order
  track.changes <- edit.history[length(edit.history):i]
  
  # apply changes
  for (y in track.changes) {
    for (j in seq(length(y$i))) {
      x[y$i[j]] <- y$a[j]
    }
  }
  
  list(
    x = x,
    edit.history = if (i-1 > 0) edit.history[1:(i-1)] else list()
  )
}
