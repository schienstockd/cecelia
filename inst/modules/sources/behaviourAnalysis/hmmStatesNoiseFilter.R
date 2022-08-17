HmmStatesNoiseFilter <- R6::R6Class(
  "HmmStatesNoiseFilter",
  inherit = BehaviourAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "hmmStatesNoiseFilter",
        sep = CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # reset image information
      # self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Calculate HMM for cells")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # TODO get tracks data.table and filter noise
      
      # noise filter states
      # TODO do this in a separate step and define
      # which states should be dominant over other states
      ### -- SNIPPET --
      # # smoothen hmm states
      # filter_order <- 5
      # 
      # # smooth clustering
      # tmp_smoothed_clustering <- cur_dat_for_hmm_filt
      # tmp_smoothed_clustering[tmp_smoothed_clustering$cell_state != 3, ]$cell_state <- 0
      # tmp_smoothed_clustering[tmp_smoothed_clustering$cell_state == 3, ]$cell_state <- 1
      # 
      # tmp_smoothed_clustering <- as.numeric(unlist(
      #   tmp_smoothed_clustering %>%
      #     arrange(EXP_ID, cell_type, TRACK_ID, POS_T_NORM) %>%
      #     group_by(EXP_ID, cell_type, TRACK_ID) %>%
      #     group_map(~ runmed(.$cell_state, k = filter_order))
      # ))
      # 
      # # smooth searching
      # tmp_smoothed_searching <- cur_dat_for_hmm_filt
      # tmp_smoothed_searching[tmp_smoothed_searching$cell_state != 2, ]$cell_state <- 0
      # tmp_smoothed_searching[tmp_smoothed_searching$cell_state == 2, ]$cell_state <- 1
      # 
      # tmp_smoothed_searching <- as.numeric(unlist(
      #   tmp_smoothed_searching %>%
      #     arrange(EXP_ID, cell_type, TRACK_ID, POS_T_NORM) %>%
      #     group_by(EXP_ID, cell_type, TRACK_ID) %>%
      #     group_map(~ runmed(.$cell_state, k = filter_order))
      # ))
      ### -- SNIPPET --
      if (self$funParams()$noiseFilterStates > 0) {
        tracks.DT[,
                  cell.state := runmed(.SD$cell.state,
                                       k = self$funParams()$noiseFilterStates),
                  by = .(cell_type, uID, track_id)
        ]
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
