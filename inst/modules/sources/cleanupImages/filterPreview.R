source(file.path(
  cfg$tasks$sources, "cleanupImages.R")
)

FilterPreview <- R6::R6Class(
  "FilterPreview",
  inherit = CleanupImages,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "filterPreview",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start filtering")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # call napari and show AF correction
      # connect to existing napari instance
      viewer <- NapariUtils$new(
        cfg$python$conda$env,
        file.path(
          cfg$python$viewer$viewerPath,
          cfg$python$viewer$connectionFile)
      )
      
      # run AF correction
      viewer$execute(
        paste(
          "import utils.python.zarr_utils as zarr_utils",
          "import utils.python.correction_utils as correction_utils",
          
          # get computing array
          "input_image = zarr_utils.get_dask_copy(napari_utils.im_data[0])",
          
          # apply filter
          "corrected_image = correction_utils.apply_filter(",
          "input_image,",
          sprintf("filter_fun = \"%s\",", self$funParams()$filterFun),
          sprintf("filter_value = %d,", self$funParams()$filterValue),
          "dim_utils = napari_utils.dim_utils",
          ")",
          sep = "\n"
        )
      )
      
      # show in napari
      viewer$showPreview(
        "corrected_image",
        channelNames = as.list(cciaObj$imChannelNames(useNames = FALSE))
      )
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      # self$updateImageInfo()
    }
  )
)