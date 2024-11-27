# Base class for import methods
Import <- R6::R6Class(
  "Import",
  inherit = TaskProcess,
  
  private = list(
  ),
  
  public = list(
    funName = function() {
      "importImages"
    },
    
    # run pre import functions
    preImport = function() {
      # create dir
      dir.create(self$envParams()$dirs$zero)
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # reset modified image
      oriFilepath <- cciaObj$oriFilepath()
      attr(oriFilepath, "modified") <- NULL
      cciaObj$setOriFilepath(oriFilepath)
      
      # save object
      cciaObj$saveState()
      
      # create MIP?
      if (self$funParams()$createMIP == TRUE) {
        self$runTasks(c(
          "importImages.createMip"
        ))
      }
      
      # # normalise image?
      # if (self$funParams()$normaliseImage == TRUE) {
      #   self$runTasks(c(
      #     "import.normaliseImage"
      #   ))
      # }
      
      # rescale image?
      if (self$funParams()$rescaleImage == TRUE) {
        self$runTasks(c(
          "importImages.rescaleImage"
        ))
      }
    },
    
    # reset image information
    resetImageInfo = function() {
      # unlink files in zero dir
      if (!"imType" %in% names(self$funParams()) || self$funParams()$imType == "primary") {
        unlink(
          file.path(self$envParams("local")$dirs$zero,
                    paste0(fileIMAGE_CONVERTED, c(".zarr", ".ome.tiff", ".ome.zarr"))),
          recursive = TRUE
        )
      }
    },
    
    # update image information after conversion
    updateImageInfo = function(imFilepath = NULL) {
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # check type of image to be imported to # set filename
      if ("imType" %in% names(self$funParams()) && self$funParams()$imType == "segmentation") {
        cciaObj$setImLabelsFilepath(imFilepath, valueName = self$funParams()$valueName)
      } else {
        # get converted image filename
        imFilepath <- Sys.glob(file.path(
          self$envParams()$dirs$zero,
          paste0(fileIMAGE_CONVERTED, "*")))
        
        cciaObj$setImFilepath(basename(imFilepath))
      
        # switch dimensions back if new order was set
        if ("dimOrder" %in% names(self$funParams()) && self$funParams()$dimOrder != "") {
          # prepare params
          params <- list(
            imPath = file.path(
              self$envParams()$dirs$zero,
              basename(cciaObj$imFilepath())
            )
          )
          
          # call python
          self$pyScript("switch_dim_order", params)
        }
        
        # delete MIP?
        if ("createMIP" %in% names(self$funParams()) && self$funParams()$createMIP == TRUE) {
          if (!is.null(cciaObj$oriFilepath(modified = TRUE)))
            unlink(cciaObj$oriFilepath(modified = TRUE))
        }
        
        # delete Rescale?
        if ("rescaleImage" %in% names(self$funParams()) && self$funParams()$rescaleImage == TRUE) {
          # make sure the image is float
          params <- list(
            imPath = file.path(
              self$envParams()$dirs$zero,
              basename(cciaObj$imFilepath())
            ),
            pixelType = "uint16"
          )
          
          # call python
          self$pyScript("change_pixel_type", params)
          
          if (!is.null(cciaObj$oriFilepath(modified = TRUE)))
            unlink(cciaObj$oriFilepath(modified = TRUE))
        }
        
        # add label properties
        # TODO this is hard coded
        if ("syncFile" %in% names(self$funParams()) && self$funParams()$syncFile == "cell2location") {
          cciaObj$setImLabelPropsFilepath(
            paste0("default", cciaConf()$files$ext$labelProps),
            valueName = "default"
          )
        }
        
        # get metadata
        cciaObj$resetMetaFromFile(reset = TRUE)
      }
        
      # save object
      cciaObj$saveState()
    }
  )
)
