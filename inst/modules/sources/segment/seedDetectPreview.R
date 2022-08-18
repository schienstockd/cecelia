SeedDetectPreview <- R6::R6Class(
  "SeedDetectPreview",
  inherit = Segment,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "seedDetectPreview",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start seed detection ")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # call napari and show seed detection
      # connect to existing napari instance
      viewer <- NapariUtils$new(useConnectionFile = TRUE)
      
      # run AF correction
      viewer$execute(
        paste(
          "from py.local_peak_seeds_utils import LocalPeakSeedsUtils",
          
          # define params
          "params = {",
          "'ccia': {",
          sprintf("'logfile': \"%s\",", self$getTaskLogFile()),
          "'append_log': True",
          "},",
          "'task_dir': napari_utils.task_dir,",
          sprintf("'im_path': \"%s\",", cciaObj$imFilepath()),
          "'dim_utils': napari_utils.dim_utils,",
          sprintf("'seed_channel': %s,", reticulate::r_to_py(self$funParams()$seedChannel)),
          sprintf("'seed_threshold_rel': %0.2f,", self$funParams()$seedThresholdRel),
          sprintf("'seed_threshold_abs': %d,", self$funParams()$seedThresholdAbs),
          sprintf("'z_spread': %d,", self$funParams()$zSpread),
          sprintf("'cell_radius': %d,", self$funParams()$cellRadius),
          sprintf("'cell_min_distance': %d,", self$funParams()$cellMinDistance),
          sprintf("'timepoints': list(range(%d, %d))",
                  self$funParams()$timepoints[[1]],
                  self$funParams()$timepoints[[2]]
                  ),
          "}",
          
          # run seed detection
          "seed_utils = LocalPeakSeedsUtils(params)",
          "seeds = seed_utils.detect_seeds(napari_utils.im_data[0])",
          sep = "\n"
        )
      )
      
      # show in napari
      viewer$showPreview(
        "seeds",
        channelNames = "Seeds",
        asPoints = TRUE
        )
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      # self$updateImageInfo()
    }
  )
)
