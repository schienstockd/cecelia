AfCorrectPreview <- R6::R6Class(
  "AfCorrectPreview",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "afCorrectPreview",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start autofluorescence correction")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      self$writeLog("Get Viewer")
      
      # call napari and show AF correction
      # connect to existing napari instance
      viewer <- self$napariViewer()
      
      # convert AF combination list to integer
      afCombinations <- self$funParams()$afCombinations
      afCombinations <- lapply(
        afCombinations, function(x) {
          x$divisionChannels <- lapply(x$divisionChannels, as.integer)
          
          x
        })
      
      self$writeLog("Apply correction")
      
      # run AF correction
      viewer$execute(
        paste(
          "import py.zarr_utils as zarr_utils",
          "import py.correction_utils as correction_utils",
          
          # get computing array
          "input_image = zarr_utils.get_dask_copy(napari_utils.im_data[0])",
          
          # convert names to integer
          sprintf("af_combinations = %s", reticulate::r_to_py(afCombinations)),
          
          # correct af channels
          "corrected_image = correction_utils.af_correct_image(",
          "input_image,",
          "af_combinations,",
          "dim_utils = napari_utils.dim_utils)",
          sep = "\n"
        )
      )
      
      channelNames <- cciaObj$imChannelNames()
      
      # add correction channel if needed
      if (!"AF generated" %in% names(channelNames)) {
        channelNames <- c(channelNames, "AF generated")
      }
      
      # remove names and convert to list
      channelNames <- as.list(channelNames)
      names(channelNames) <- NULL
      
      self$writeLog("Show Preview")
      
      # show in napari
      viewer$showPreview(
        "corrected_image",
        channelNames = channelNames,
        multiscale = FALSE
      )
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      # self$updateImageInfo()
    }
  )
)
