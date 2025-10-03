SplitIntoCoresPreview <- R6::R6Class(
  "SplitIntoCoresPreview",
  inherit = Import,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "splitIntoCoresPreview",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start cores detection")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # call napari and show AF correction
      # connect to existing napari instance
      viewer <- self$napariViewer()
      
      # run cores detection
      viewer$execute(
        paste(
          "import py.opal_utils as opal_utils",
          "import py.zarr_utils as zarr_utils",
          
          # load image
          sprintf(
            "im_dat, _ = zarr_utils.open_image_as_zarr('%s', as_dask = True)",
            cciaObj$oriFilepath()
          ),
          
          # get label im
          sprintf(
            paste(
              "label_im, im = opal_utils.extract_cores_labels(",
              "im_dat = im_dat,",
              "median_filter = %d,",
              "otsu_adjust = %0.2f,",
              "closing_filter = %d,",
              "small_objects_size = %d,",
              "label_expansion = %d",
              ")"
              ),
            self$funParams()$medianFilter,
            self$funParams()$otsuAdjust,
            self$funParams()$closingFilter,
            self$funParams()$smallObjectsSize,
            self$funParams()$labelExpansion
          ),
          sep = "\n"
        )
      )
      
      # reset viewer
      viewer$openViewer()
      viewer$clearViewer()
      
      # add image
      viewer$showPreview(
        "im", channelNames = c("cores"),
        multiscale = FALSE, useChannelAxis = FALSE,
        useScale = FALSE
      )
      
      # add labels
      viewer$showPreview(
        "label_im", channelNames = c("labels"),
        multiscale = FALSE, useChannelAxis = FALSE,
        useScale = FALSE, asLabels = TRUE
      )
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      # self$updateImageInfo()
    }
  )
)
