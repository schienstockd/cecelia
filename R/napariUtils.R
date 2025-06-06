#' Napari utils
#' 
#' @name NapariUtils
#' @description Napari utils
#'
#' @examples
#' TODO
#' @export
NapariUtils <- R6::R6Class(
  "NapariUtils",
  inherit = JupyterKernelUtils,
  
  ### public
  public = list(
    #' @description Init napari
    #' @param viewerOutputFile character for output file path
    #' @param viewerInputFile character for input file path
    #' @param execInteractive boolean to execute interactive
    initNapari = function(viewerOutputFile = NULL, viewerInputFile = NULL, execInteractive = TRUE) {
      # check files
      if (is.null(viewerOutputFile))
        viewerOutputFile <- file.path(
          cciaPath(), "app",
          cciaConf()$python$viewer$viewerPath,
          cciaConf()$python$viewer$outputFile
        )
      if (is.null(viewerInputFile))
        viewerInputFile <- file.path(
          cciaPath(), "app",
          cciaConf()$python$viewer$viewerPath,
          cciaConf()$python$viewer$inputFile
        )
      
      # import napari
      self$execute(paste(
        "import numpy as np",
        "from py.napari_utils import NapariUtils",
        "init_napari = True",
        "try:",
        "\tinit_napari = napari_utils is None",
        "except NameError:",
        "\tpass",
        "if init_napari is True: napari_utils = NapariUtils()",
        sep = "\n"
      ), execInteractive = execInteractive)
      
      # set io files
      private$setViewerOutputFile(normalizePath(viewerOutputFile))
      private$setViewerInputFile(normalizePath(viewerInputFile))
      
      # clear user input
      self$clearViewerInput()
    },
    
    #' @description Add pixel classifier
    #' TODO this also required the napari extension
    #' Should this be installed here if not present?
    #' @param execInteractive boolean to execute interactive
    addPixclPane = function(execInteractive = TRUE) {
      self$execute(
        paste(
          "_, widget = napari_utils.viewer.window.add_plugin_dock_widget(",
          "'napari-accelerated-pixel-and-object-classification',",
          "widget_name = 'Semantic Segmentation'",
          ")", sep = "\n"
        ), execInteractive = execInteractive)
    },
    
    #' @description Add animation pane
    #' TODO this also required the napari extension
    #' Should this be installed here if not present?
    #' @param execInteractive boolean to execute interactive
    addAnimationPane = function(execInteractive = TRUE) {
      self$execute(
        paste(
          "from napari_animation import AnimationWidget",
          "animation_widget = AnimationWidget(napari_utils.viewer)",
          "napari_utils.viewer.window.add_dock_widget(animation_widget, area='right')",
          sep = "\n"
        ), execInteractive = execInteractive)
    },
    
    #' @description Open viewer
    #' @param execInteractive boolean to execute interactive
    openViewer = function(execInteractive = TRUE) {
      self$execute(
        "if napari_utils.viewer is None: napari_utils.open_viewer()",
        execInteractive = execInteractive)
    },
    
    #' @description Close viewer
    #' @param execInteractive boolean to execute interactive
    closeViewer = function(execInteractive = TRUE) {
      self$execute(
        "if napari_utils.viewer is not None: napari_utils.close_viewer()",
        execInteractive = execInteractive)
    },
    
    #' @description Clear viewer
    #' @param execInteractive boolean to execute interactive
    clearViewer = function(execInteractive = TRUE) {
      self$execute(
        "napari_utils.viewer.layers.clear()",
        execInteractive = execInteractive)
    },
    
    #' @description Clear viewer output
    clearViewerOutput = function() {
      write(NULL, private$getViewerOutputFile())
    },
    
    #' @description Clear viewer input
    clearViewerInput = function() {
      write(NULL, private$getViewerInputFile())
    },
    
    #' @description Open image
    #' @param imPath character for image path
    #' @param useChannelAxis boolean to use channel axis
    #' @param imChannelNames list of character for channel names
    #' @param channelColormaps list of character for channel colormaps
    #' @param napariModule character to add napari module
    #' @param asDask boolean to load image as dask
    #' @param downsampleZ boolean to downsample image
    #' @param show3D boolean to show image in 3D
    #' @param multiscales integer for multiscales
    #' @param execInteractive boolean to execute interactive
    #' @param layersVisible boolean to make layers visible
    #' @param squeeze boolean to load image as squeezed numpy array
    #' @param showTimestamp boolean to show time interval
    #' @param timeInterval numeric for timelapse interval
    #' @param resetViewer reset viewer before opening image
    openImage = function(
      imPath, useChannelAxis = TRUE, imChannelNames = NULL, channelColormaps = NULL,
      napariModule = NULL, asDask = TRUE, downsampleZ = FALSE, show3D = FALSE,
      multiscales = NULL, execInteractive = TRUE, layersVisible = TRUE,
      squeeze = FALSE, showTimestamp = FALSE, timeInterval = 1, resetViewer = TRUE) {
      # map path
      imPath <- .dockerMapPathToHost(imPath)
      
      # clear and open image
      self$clearViewerInput()
      self$clearViewerOutput()
      
      # reset viewer
      if (resetViewer == TRUE) {
        self$closeViewer()
        self$openViewer()
      } else {
        # add basename to channel names
        imChannelNames <- paste(imChannelNames, paste0("(", basename(imPath), ")"))
      }
      
      # open image
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.open_image(r'%s',",
            "use_channel_axis=%s,",
            "channel_names=%s,",
            "channel_colormaps=%s,",
            "multiscales=%s,",
            "as_dask=%s,",
            "downsample_z=%s,",
            "show_3D=%s,",
            "visible=%s,",
            "squeeze=%s",
            ")"
          ),
          imPath,
          reticulate::r_to_py(useChannelAxis),
          # unlist - otherwise the names of the channels will be used 
          # List (16 items) will be used if there are many names ..
          # reticulate::r_to_py(forceAsFlatList(imChannelNames)), 
          sprintf("['%s']", paste(c(unname(imChannelNames)), collapse = "', '")),
          # reticulate::r_to_py(channelColormaps),
          if (length(channelColormaps) > 0) sprintf("['%s']", paste(c(unname(channelColormaps)), collapse = "', '")) else "None",
          if (!is.null(multiscales)) reticulate::r_to_py(as.integer(multiscales)) else reticulate::r_to_py(NULL),
          reticulate::r_to_py(asDask),
          reticulate::r_to_py(downsampleZ),
          reticulate::r_to_py(show3D),
          # reticulate::r_to_py(layersVisible),
          if (length(layersVisible) > 1) sprintf("[%s]", paste(sapply(layersVisible, function(x) if (x) "True" else "False"), collapse = ", ")) else reticulate::r_to_py(layersVisible),
          reticulate::r_to_py(squeeze)
          ), execInteractive = execInteractive
        )
      
      # add timestamp if movie
      if (showTimestamp == TRUE) {
        self$execute(
          sprintf(
            paste("napari_utils.add_timestamp(time_interval=%s)"),
            reticulate::r_to_py(timeInterval)
          ), execInteractive = execInteractive
        )
      }
      
      # define module specifics
      if (!is.null(napariModule)) {
        self$execute(
          sprintf(
            "if hasattr(napari_utils, 'create_%1$s_module'): napari_utils.create_%1$s_module()",
            napariModule
          ), execInteractive = execInteractive
        )
      }
    },
    
    #' @description Save layer properties
    #' @param layerFile character for filepath to save props
    #' @param execInteractive boolean to execute interactive
    saveLayerProps = function(layerFile, execInteractive = TRUE) {
      self$execute(
        paste(
          "if napari_utils.viewer is not None: napari_utils.save_layer_props(",
          sprintf("filepath = '%s'", reticulate::r_to_py(layerFile)),
          ")"
        ), execInteractive = execInteractive)
    },
    
    #' @description Load layer properties
    #' @param layerFile character for filepath to load props
    #' @param execInteractive boolean to execute interactive
    loadLayerProps = function(layerFile, execInteractive = TRUE) {
      self$execute(
        paste(
          "if napari_utils.viewer is not None: napari_utils.load_layer_props(",
          sprintf("filepath = '%s'", reticulate::r_to_py(layerFile)),
          ")"
        ), execInteractive = execInteractive)
    },
    
    #' @description Reset labels scale
    #' @param layersStartswith character to match layers
    #' @param execInteractive boolean to execute interactive
    resetLabelsScale = function(layersStartswith = NULL, execInteractive = TRUE) {
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.reset_labels_scale(",
            sprintf("layers_startswith = %s,", if (!is.null(layersStartswith)) "'%s'" else "%s"),
            ")"
          ),
          reticulate::r_to_py(layersStartswith)
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Save labels
    #' @param pathToFile character for labels path
    #' @param layerName character for layer name
    #' @param excludeNames boolean to exclude names
    #' @param notifyModuleID character to notify a module once labels are saved
    #' @param execInteractive boolean to execute interactive
    saveLabels = function(pathToFile, layerName = "Labels",
                          excludeNames = NULL, notifyModuleID = NULL,
                          execInteractive = TRUE) {
      # save labels back
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.save_labels(",
            "filepath = r'%s',",
            "layer_name = '%s',",
            sprintf("exclude_names = %s,", if (!is.null(excludeNames)) "'%s'" else "%s"),
            sprintf("notify_module_id = %s,", if (!is.null(notifyModuleID)) "'%s'" else "%s"),
            ")"
          ),
          # reticulate::r_to_py(pathToFile),
          reticulate::r_to_py(.dockerMapPathToHost(pathToFile)),
          reticulate::r_to_py(layerName),
          reticulate::r_to_py(excludeNames),
          reticulate::r_to_py(notifyModuleID)
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Show labels all
    #' @param valueNames list of character for value names
    #' @param showLabels boolean to show labels
    #' @param showPoints boolean to show points
    #' @param showLabelIds boolean to show label ids
    #' @param showBranching boolean to show branching
    #' @param branchingProperty character of branching property to show
    #' @param showTracks boolean to show tracks
    #' @param showPops boolean to show populations
    #' @param showNeighbours boolean to show neighbours
    #' @param asNpArray boolean to load labels as numpy array for editing
    #' @param execInteractive boolean to execute interactive
    #' @param splitTracks list to split tracks by properties
    #' @param tracksBlending character to set blending for tracks
    showLabelsAll = function(valueNames, showLabels = FALSE, showPoints = FALSE,
                             showLabelIds = FALSE, showTracks = FALSE,
                             showPops = FALSE, showNeighbours = FALSE,
                             showBranching = FALSE, branchingProperty = "type",
                             asNpArray = FALSE, execInteractive = TRUE,
                             labelSuffixes = list(), splitTracks = NULL, 
                             tracksBlending = "additive") {
      # show labels
      if (length(valueNames) > 0) {
        self$execute(
          sprintf(
            paste(
              "if napari_utils.viewer is not None: napari_utils.show_labels_all(",
              "value_names = %s,",
              "show_labels = %s,",
              "show_points = %s,",
              "show_label_ids = %s,",
              "show_tracks = %s,",
              "show_branching = %s,",
              "branching_property = '%s',",
              "as_np_array = %s,",
              "label_suffixes = %s,",
              "split_tracks = %s,",
              "tracks_blending = '%s'",
              ")"
            ),
            reticulate::r_to_py(valueNames),
            reticulate::r_to_py(showLabels),
            reticulate::r_to_py(showPoints),
            reticulate::r_to_py(showLabelIds),
            reticulate::r_to_py(showTracks),
            reticulate::r_to_py(showBranching),
            reticulate::r_to_py(branchingProperty),
            reticulate::r_to_py(asNpArray),
            reticulate::r_to_py(labelSuffixes),
            reticulate::r_to_py(splitTracks),
            tracksBlending
          ), execInteractive = execInteractive
        )
      }
    },
    
    #' @description Show labels
    #' @param showLabels boolean to show labels
    #' @param showPoints boolean to show points
    #' @param showLabelIds boolean to show label ids
    #' @param showTracks boolean to show tracks
    #' @param showBranching boolean to show branching
    #' @param asNpArray boolean to load labels as numpy array for editing
    #' @param valueName character for value name
    #' @param labelSuffixes character for label suffixes
    #' @param execInteractive boolean to execute interactive
    showLabels = function(showLabels = TRUE, showPoints = TRUE, showLabelIds = FALSE,
                          showTracks = TRUE, asNpArray = FALSE,
                          valueName = NULL, labelSuffixes = list(),
                          execInteractive = TRUE) {
      # get patterns for sprintf
      valueNamePattern <- if (is.null(valueName)) "%s" else "\"%s\""
      
      # show labels
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.show_labels(",
            "show_labels = %s,",
            "show_points = %s,",
            "show_label_ids = %s,",
            "show_tracks = %s,",
            "show_branching = %s,",
            paste0("value_name = ", valueNamePattern, ","),
            "as_np_array = %s,",
            "label_suffixes = %s",
            ")"
          ),
          reticulate::r_to_py(showLabels),
          reticulate::r_to_py(showPoints),
          reticulate::r_to_py(showLabelIds),
          reticulate::r_to_py(showTracks),
          reticulate::r_to_py(showBranching),
          reticulate::r_to_py(valueName),
          reticulate::r_to_py(asNpArray),
          reticulate::r_to_py(labelSuffixes)
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Highlight tracks
    #' @param valueName character for value name
    #' @param trackIDs list of integer for track IDs
    #' @param name character for layer name
    #' @param execInteractive boolean to execute interactive
    highlightTracks = function(valueName, trackIDs, name = "*Tracks", execInteractive = FALSE) {
      # highlight tracks
      self$execute(
        sprintf(
          "if napari_utils.viewer is not None: napari_utils.highlight_tracks('%s', %s, '%s')",
          valueName,
          sprintf("[%s]", paste(trackIDs, collapse = ",")),
          name
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Centre camera
    #' @param pos numeric list of XY position
    #' @param tp numeric for timepoint
    #' @param zoom numeric for zoom
    #' @param execInteractive boolean to execute interactive
    centre = function(pos, tp = NULL, zoom = NULL, execInteractive = FALSE) {
      # centre camera
      self$execute(
        sprintf(
          "if napari_utils.viewer is not None: napari_utils.centre(%s, %s, %s)",
          reticulate::r_to_py(pos), tp, zoom
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Save screenshot
    #' @param filePath character to save screenshot
    #' @param size list for resolution
    #' @param scale numeric to scale image resolution
    #' @param canvasOnly boolean to save only canvas
    #' @param execInteractive boolean to execute interactive
    saveScreenshot = function(filePath, size = NULL, scale = NULL,
                              canvasOnly = TRUE, execInteractive = TRUE) {
      # save screenshot
      self$execute(
        sprintf(
          paste(
            "napari_utils.viewer.window.screenshot(r'%s',",
            "size = %s,",
            "scale = %s,",
            "flash = %s,",
            "canvas_only = %s",
            ")",
            sep = "\n"
          ),
          filePath,
          reticulate::r_to_py(size),
          reticulate::r_to_py(scale),
          reticulate::r_to_py(canvasOnly),
          reticulate::r_to_py(TRUE)
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Save animation
    #' @param moviePath character to save animation
    #' @param fps integer frames per second
    #' @param windowSizeX integer to set napari window size
    #' @param windowSizeXY integer to set napari window size
    #' @param canvasOnly boolean to save only canvas
    #' @param execInteractive boolean to execute interactive
    saveTimeAnimation = function(moviePath, fps = 10, windowSizeX = 1200,
                                 windowSizeY = 1000, canvasOnly = TRUE,
                                 execInteractive = TRUE) {
      # save animation
      self$execute(
        paste(
          "from napari_animation import Animation",
          "from napari_animation.easing import Easing",
          
          # resize window
          sprintf("napari_utils.viewer.window.resize(%i, %i)",
                  windowSizeX, windowSizeY),
          "napari_utils.viewer.reset_view()",
          
          "animation = Animation(napari_utils.viewer)",
          
          # first
          "dim_array = np.zeros_like(napari_utils.dim_utils.dim_vals(ignore_channel = True))",
          "t_val = napari_utils.dim_utils.dim_val('T')",
          "napari_utils.viewer.dims.current_step = list(dim_array)",
          "animation.capture_keyframe(steps = 0)",
  
          # last
          "dim_array[napari_utils.dim_utils.dim_idx('T', ignore_channel = True)] = t_val - 1",
          "napari_utils.viewer.dims.current_step = list(dim_array)",
          "animation.capture_keyframe(steps = t_val - 2)",
          
          # save back
          sprintf("animation.animate(r'%s', canvas_only = %s, fps = %i)",
                  moviePath, reticulate::r_to_py(canvasOnly), fps),
          sep = "\n"
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Highlight labels
    #' @param valueName character for value name
    #' @param labelIDs list of integer for label IDs
    #' @param execInteractive boolean to execute interactive
    highlightLabels = function(valueName, labelIDs, execInteractive = FALSE) {
      # highlight labels
      self$execute(
        sprintf(
          "if napari_utils.viewer is not None: napari_utils.highlight_labels('%s', %s)",
          valueName,
          sprintf("[%s]", paste(labelIDs, collapse = ","))
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Show channel intensities
    #' @param channelID integer for channel id
    #' @param execInteractive boolean to execute interactive
    showChannelIntensity = function(channelID, execInteractive = FALSE) {
      # show channel intensity
      self$execute(
        sprintf(
          "if napari_utils.viewer is not None: napari_utils.show_channel_intensity(%d)",
          channelID
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Show layer
    #' @param layerName character for layer naem
    #' @param execInteractive boolean to execute interactive
    showLayer = function(layerName, execInteractive = FALSE) {
      self$execute(
        sprintf(
          "napari_utils.show_layer('%s')",
          layerName
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Hide layer
    #' @param layerName character for layer naem
    #' @param execInteractive boolean to execute interactive
    hideLayer = function(layerName, execInteractive = FALSE) {
      self$execute(
        sprintf(
          "napari_utils.hide_layer('%s')",
          layerName
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Set points size for populations
    #' @param popType character for population type
    #' @param pointSize integer for point size
    #' @param execInteractive boolean to execute interactive
    setPopPointsSize = function(popType, pointSize, execInteractive = FALSE) {
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.set_pop_points_size(",
            "pop_type = '%s',",
            "point_size = %s",
            ")"
          ),
          reticulate::r_to_py(popType),
          reticulate::r_to_py(pointSize)
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Show population mapping
    #' @param popType character for population type
    #' @param valueName character for value name
    #' @param removePrevious boolean to remove previous populations
    #' @param filteredFromValueName boolean to filter from value name
    #' @param pointSize integer for point size
    #' @param execInteractive boolean to execute interactive
    showPopMapping = function(popType, valueName = NULL,
                              removePrevious = TRUE,
                              filteredFromValueName = FALSE,
                              pointsSize = 8, pops = list(),
                              execInteractive = TRUE) {
      self$execute(
        # show mapping
        sprintf(
          paste(
            "napari_utils.show_pop_mapping('%s',",
            if (is.null(valueName)) "value_name = %s," else "value_name = '%s',",
            "remove_previous = %s,",
            "filtered_from_value_name = %s,",
            "pops = %s,",
            "points_size = %s",
            ")",
            sep = "\n"
          ),
          popType,
          reticulate::r_to_py(valueName),
          reticulate::r_to_py(removePrevious),
          reticulate::r_to_py(filteredFromValueName),
          reticulate::r_to_py(pops),
          reticulate::r_to_py(pointsSize)
        ), execInteractive = execInteractive
      )
    },
    
    #' @description Show cell neighbours
    #' @param popType character for population type
    #' @param valueName character for value name
    #' @param removePrevious boolean to remove previous populations
    #' @param execInteractive boolean to execute interactive
    showCellNeighbours = function(popType, valueName = "default",
                                  removePrevious = TRUE,
                                  execInteractive = FALSE) {
      self$execute(
        # show neigbours
        paste(
          sprintf("napari_utils.show_cell_neighbours('%s',", popType),
          if (is.null(valueName))
            sprintf("value_name = %s,", reticulate::r_to_py(valueName))
          else
            sprintf("value_name = '%s',", reticulate::r_to_py(valueName)),
          sprintf("remove_previous = %s,",
                  reticulate::r_to_py(removePrevious)),
          ")",
          sep = "\n"),
        execInteractive = execInteractive
      )
    },
    
    #' @description Save shapes
    # TODO this needs more parameters
    #' @param shapeType character for shape type
    #' @param valueName character for value name
    #' @param execInteractive boolean to execute interactive
    saveShapes = function(shapeType = "region", valueName = "default", execInteractive = FALSE) {
      self$execute(
        # save shapes
        paste(
          "napari_utils.save_shapes(",
          sprintf("shape_type = '%s',", reticulate::r_to_py(shapeType)),
          sprintf("value_name = '%s'", reticulate::r_to_py(valueName)),
          ")",
          sep = "\n"),
        execInteractive = execInteractive
      )
    },
    
    #' @description Show shapes
    # TODO this needs more parameters
    #' @param shapeType character for shape type
    #' @param valueName character for value name
    #' @param removePrevious boolean to remove previous
    #' @param execInteractive boolean to execute interactive
    showShapes = function(
      shapeType = 'region', valueName = "default",
      removePrevious = TRUE, execInteractive = FALSE) {
      self$execute(
        # show shapes
        paste(
          "napari_utils.show_shapes(",
          sprintf("shape_type = '%s',", reticulate::r_to_py(shapeType)),
          sprintf("value_name = '%s',", reticulate::r_to_py(valueName)),
          sprintf("remove_previous = %s", reticulate::r_to_py(removePrevious)),
          ")",
          sep = "\n"),
        execInteractive = execInteractive
      )
    },
    
    #' @description Get population layer name
    #' @param popType character for population type
    #' @param popName character for population name
    popLayerName = function(popType, popName) {
      sprintf("(%s) %s", popType, popName)
    },
    
    #' @description Show preview
    #' @param imageArray character for image array - this is a python variable name
    #' @param channelNames list of character for channel names
    #' @param asPoints boolean to show as points
    #' @param size integer for size
    #' @param asLabels boolean to show as labels
    #' @param multiscale integer for multiscales
    #' @param execInteractive boolean to execute interactive
    #' @param useScale boolean to use scale
    #' @param useChannelAxis boolean to use channel axis
    showPreview = function(imageArray, channelNames = NULL,
                           asPoints = FALSE, size = 5,
                           asLabels = FALSE, multiscale = NULL,
                           execInteractive = TRUE, useScale = TRUE,
                           useChannelAxis = TRUE) {
      self$execute(
        paste(
          sprintf("napari_utils.show_preview(%s, ", imageArray),
          if (is.list(channelNames))
            sprintf("channel_names = %s,", reticulate::r_to_py(channelNames))
          else
            sprintf("channel_names = \"%s\",", reticulate::r_to_py(channelNames)),
          sprintf("as_points = %s,", reticulate::r_to_py(asPoints)),
          sprintf("as_labels = %s,", reticulate::r_to_py(asLabels)),
          sprintf("multiscale = %s,", reticulate::r_to_py(multiscale)),
          sprintf("use_scale = %s,", reticulate::r_to_py(useScale)),
          sprintf("use_channel_axis = %s,", reticulate::r_to_py(useChannelAxis)),
          if (asPoints == TRUE)
            sprintf("size = %d", size),
          ")",
          sep = "\n"
        ), 
        execInteractive = execInteractive
      )
    },
    
    # setters
    setTaskDir = function(x, execInteractive = TRUE) {
      # map path
      x <- .dockerMapPathToHost(x)
      
      self$execute(
        sprintf("napari_utils.task_dir = r'%s'", x),
        execInteractive = execInteractive
      )
    }
    
    ## getters
  ),
  
  ### private
  private = list(
    viewerInputFile = NULL,
    viewerOutputFile = NULL,
    
    ## setters
    setViewerOutputFile = function(x, execInteractive = TRUE) {
      self$execute(
        sprintf("napari_utils.viewer_output_file = r'%s'",
                .dockerMapPathToHost(x, path = "home")),
        execInteractive = execInteractive
      )
      
      private$viewerOutputFile <- x
    },
    
    setViewerInputFile = function(x, execInteractive = TRUE) {
      self$execute(
        sprintf("napari_utils.viewer_input_file = r'%s'",
                .dockerMapPathToHost(x, path = "home")),
        execInteractive = execInteractive
      )
      
      private$viewerInputFile <- x
    },
    
    ## getters
    getViewerOutputFile = function() {
      private$viewerOutputFile
    },
    
    getViewerInputFile = function() {
      private$viewerInputFile
    }
  )
)
