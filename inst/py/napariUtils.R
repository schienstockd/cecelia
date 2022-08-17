NapariUtils <- R6::R6Class(
  "NapariUtils",
  inherit = JupyterKernelUtils,
  
  ## public
  public = list(
    # init napari
    initNapari = function(viewerOutputFile, viewerInputFile, execInteractive = TRUE) {
      # import napari
      self$execute(paste(
        sprintf(
          "from %s import NapariUtils", "utils.python.napari_utils"
          ),
        "init_napari = True",
        "try:",
        "\tinit_napari = napari_utils is None",
        "except NameError:",
        "\tpass",
        "if init_napari is True: napari_utils = NapariUtils()",
        sep = "\n"
      ), execInteractive = execInteractive)
      
      # set io files
      private$setViewerOutputFile(viewerOutputFile)
      private$setViewerInputFile(viewerInputFile)
      
      # clear user input
      self$clearViewerInput()
    },
    
    # add pixel classifier
    addPixclPane = function(execInteractive = TRUE) {
      self$execute(
        paste(
          "_, widget = napari_utils.viewer.window.add_plugin_dock_widget(",
          "'napari-accelerated-pixel-and-object-classification',",
          "widget_name = 'Semantic Segmentation'",
          ")", sep = "\n"
        ), execInteractive = execInteractive)
    },
    
    # add animation pane
    addAnimationPane = function(execInteractive = TRUE) {
      self$execute(
        paste(
          "from napari_animation import AnimationWidget",
          "animation_widget = AnimationWidget(napari_utils.viewer)",
          "napari_utils.viewer.window.add_dock_widget(animation_widget, area='right')",
          sep = "\n"
        ), execInteractive = execInteractive)
    },
    
    # open viewer
    openViewer = function(execInteractive = TRUE) {
      self$execute(
        "if napari_utils.viewer is None: napari_utils.open_viewer()",
        execInteractive = execInteractive)
    },
    
    # close viewer
    closeViewer = function(execInteractive = TRUE) {
      self$execute(
        "if napari_utils.viewer is not None: napari_utils.close_viewer()",
        execInteractive = execInteractive)
    },
    
    # clear viewer
    clearViewer = function(execInteractive = TRUE) {
      self$execute(
        "napari_utils.viewer.layers.clear()",
        execInteractive = execInteractive)
    },
    
    # clear viewer output
    clearViewerOutput = function() {
      write(NULL, private$getViewerOutputFile())
    },
    
    # clear viewer input
    clearViewerInput = function() {
      write(NULL, private$getViewerInputFile())
    },
    
    # open image
    openImage = function(
      imPath, useChannelAxis = TRUE, imChannelNames = NULL, channelColormaps = NULL,
      napariModule = NULL, asDask = TRUE, show3D = FALSE, multiscales = NULL,
      execInteractive = TRUE, layersVisible = TRUE) {
      # map path
      imPath <- dockerMapPathToHost(imPath)
      
      # clear and open image
      self$clearViewerInput()
      self$clearViewerOutput()
      
      # reset viewer
      self$closeViewer()
      self$openViewer()
      
      # open image
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.open_image('%s',",
            "use_channel_axis=%s,",
            "channel_names=%s,",
            "channel_colormaps=%s,",
            "multiscales=%s,",
            "as_dask=%s,",
            "show_3D=%s,",
            "visible=%s",
            ")"
          ),
          imPath,
          r_to_py(useChannelAxis),
          # unlist - otherwise the names of the channels will be used 
          # List (16 items) will be used if there are many names ..
          # r_to_py(forceAsFlatList(imChannelNames)), 
          sprintf("['%s']", paste(c(unname(imChannelNames)), collapse = "', '")),
          r_to_py(channelColormaps),
          r_to_py(multiscales),
          r_to_py(asDask),
          r_to_py(show3D),
          r_to_py(layersVisible)
          ), execInteractive = execInteractive
        )
      
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
    
    # reset labels scale
    resetLabelsScale = function(layersStartswith = NULL, execInteractive = TRUE) {
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.reset_labels_scale(",
            sprintf("layers_startswith = %s,", if (!is.null(layersStartswith)) "'%s'" else "%s"),
            ")"
          ),
          r_to_py(layersStartswith)
        ), execInteractive = execInteractive
      )
      
    },
    
    # save labels
    saveLabels = function(pathToFile, layerName = "Labels",
                          excludeNames = NULL, notifyModuleID = NULL,
                          execInteractive = TRUE) {
      # save labels back
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.save_labels(",
            "filepath = '%s',",
            "layer_name = '%s',",
            sprintf("exclude_names = %s,", if (!is.null(excludeNames)) "'%s'" else "%s"),
            sprintf("notify_module_id = %s,", if (!is.null(notifyModuleID)) "'%s'" else "%s"),
            ")"
          ),
          r_to_py(pathToFile), r_to_py(layerName),
          r_to_py(excludeNames), r_to_py(notifyModuleID)
        ), execInteractive = execInteractive
      )
    },
    
    # show labels all
    showLabelsAll = function(valueNames, showLabels = TRUE, showPoints = TRUE,
                             showTracks = TRUE, showPops = TRUE, showNeighbours = TRUE,
                             asNpArray = FALSE, execInteractive = TRUE,
                             labelSuffixes = list()) {
      # show labels
      if (length(valueNames) > 0) {
        self$execute(
          sprintf(
            paste(
              "if napari_utils.viewer is not None: napari_utils.show_labels_all(",
              "value_names = %s,",
              "show_labels = %s,",
              "show_points = %s,",
              "show_tracks = %s,",
              "as_np_array = %s,",
              "label_suffixes = %s",
              ")"
            ),
            r_to_py(valueNames),
            r_to_py(showLabels),
            r_to_py(showPoints),
            r_to_py(showTracks),
            r_to_py(asNpArray),
            r_to_py(labelSuffixes)
          ), execInteractive = execInteractive
        )
      }
    },
    
    # show labels
    showLabels = function(showLabels = TRUE, showPoints = TRUE,
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
            "show_tracks = %s,",
            paste0("value_name = ", valueNamePattern, ","),
            "as_np_array = %s,",
            "label_suffixes = %s",
            ")"
          ),
          r_to_py(showLabels),
          r_to_py(showPoints),
          r_to_py(showTracks),
          r_to_py(valueName),
          r_to_py(asNpArray),
          r_to_py(labelSuffixes)
        ), execInteractive = execInteractive
      )
    },
    
    # highlight tracks
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
    
    # highlight labels
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
    
    # show channel intensities
    showChannelIntensity = function(channelID, execInteractive = FALSE) {
      # show channel intensity
      self$execute(
        sprintf(
          "if napari_utils.viewer is not None: napari_utils.show_channel_intensity(%d)",
          channelID
        ), execInteractive = execInteractive
      )
    },
    
    # show layer
    showLayer = function(layerName, execInteractive = FALSE) {
      self$execute(
        sprintf(
          "napari_utils.show_layer('%s')",
          layerName
        ), execInteractive = execInteractive
      )
    },
    
    # hide layer
    hideLayer = function(layerName, execInteractive = FALSE) {
      self$execute(
        sprintf(
          "napari_utils.hide_layer('%s')",
          layerName
        ), execInteractive = execInteractive
      )
    },
    
    
    # set points size for populations
    setPopPointsSize = function(popType, pointSize, execInteractive = FALSE) {
      self$execute(
        sprintf(
          paste(
            "if napari_utils.viewer is not None: napari_utils.set_pop_points_size(",
            "pop_type = '%s',",
            "point_size = %s",
            ")"
          ),
          r_to_py(popType),
          r_to_py(pointSize)
        ), execInteractive = execInteractive
      )
    },
    
    # show population mapping
    showPopMapping = function(popType, valueName = NULL,
                              removePrevious = TRUE,
                              filteredFromValueName = FALSE,
                              pointsSize = 6,
                              execInteractive = FALSE) {
      self$execute(
        # show mapping
        paste(
          sprintf("napari_utils.show_pop_mapping('%s',", popType),
          if (is.null(valueName))
            sprintf("value_name = %s,", r_to_py(valueName))
          else
            sprintf("value_name = '%s',", r_to_py(valueName)),
          sprintf("remove_previous = %s,",
                  r_to_py(removePrevious)),
          sprintf("filtered_from_value_name = %s,",
                  r_to_py(filteredFromValueName)),
          sprintf("points_size = %s", r_to_py(pointsSize)),
          ")",
          sep = "\n"),
        execInteractive = execInteractive
      )
    },
    
    # show cell neighbours
    showCellNeighbours = function(popType, valueName = "default",
                              removePrevious = TRUE,
                              execInteractive = FALSE) {
      self$execute(
        # show neigbours
        paste(
          sprintf("napari_utils.show_cell_neighbours('%s',", popType),
          if (is.null(valueName))
            sprintf("value_name = %s,", r_to_py(valueName))
          else
            sprintf("value_name = '%s',", r_to_py(valueName)),
          sprintf("remove_previous = %s,",
                  r_to_py(removePrevious)),
          ")",
          sep = "\n"),
        execInteractive = execInteractive
      )
    },
    
    # save shapes
    # TODO this needs more parameters
    saveShapes = function(shapeType = 'region', valueName = "default", execInteractive = FALSE) {
      self$execute(
        # save shapes
        paste(
          "napari_utils.save_shapes(",
          sprintf("shape_type = '%s',", r_to_py(shapeType)),
          sprintf("value_name = '%s'", r_to_py(valueName)),
          ")",
          sep = "\n"),
        execInteractive = execInteractive
      )
    },
    
    # show shapes
    # TODO this needs more parameters
    showShapes = function(
      shapeType = 'region', valueName = "default",
      removePrevious = TRUE, execInteractive = FALSE) {
      self$execute(
        # show shapes
        paste(
          "napari_utils.show_shapes(",
          sprintf("shape_type = '%s',", r_to_py(shapeType)),
          sprintf("value_name = '%s',", r_to_py(valueName)),
          sprintf("remove_previous = %s", r_to_py(removePrevious)),
          ")",
          sep = "\n"),
        execInteractive = execInteractive
      )
    },
    
    # get population layer name
    popLayerName = function(popType, popName) {
      sprintf("(%s) %s", popType, popName)
    },
    
    # show preview
    showPreview = function(imageArray, channelNames = NULL,
                           asPoints = FALSE, size = 5,
                           asLabels = FALSE, multiscale = NULL,
                           execInteractive = TRUE, useScale = TRUE,
                           useChannelAxis = TRUE) {
      self$execute(
        paste(
          sprintf("napari_utils.show_preview(%s, ", imageArray),
          if (is.list(channelNames))
            sprintf("channel_names = %s,", r_to_py(channelNames))
          else
            sprintf("channel_names = \"%s\",", r_to_py(channelNames)),
          sprintf("as_points = %s,", r_to_py(asPoints)),
          sprintf("as_labels = %s,", r_to_py(asLabels)),
          sprintf("multiscale = %s,", r_to_py(multiscale)),
          sprintf("use_scale = %s,", r_to_py(useScale)),
          sprintf("use_channel_axis = %s,", r_to_py(useChannelAxis)),
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
      x <- dockerMapPathToHost(x)
      
      self$execute(
        sprintf("napari_utils.task_dir = '%s'", x),
        execInteractive = execInteractive
      )
    }
    
    # getters
    
  ),
  
  ## private
  private = list(
    viewerInputFile = NULL,
    viewerOutputFile = NULL,
    
    # setters
    setViewerOutputFile = function(x, execInteractive = TRUE) {
      self$execute(
        sprintf("napari_utils.viewer_output_file = '%s'", x),
        execInteractive = execInteractive
      )
      
      private$viewerOutputFile <- x
    },
    
    setViewerInputFile = function(x, execInteractive = TRUE) {
      self$execute(
        sprintf("napari_utils.viewer_input_file = '%s'", x),
        execInteractive = execInteractive
      )
      
      private$viewerInputFile <- x
    },
    
    # getters
    getViewerOutputFile = function() {
      private$viewerOutputFile
    },
    
    getViewerInputFile = function() {
      private$viewerInputFile
    }
  )
)