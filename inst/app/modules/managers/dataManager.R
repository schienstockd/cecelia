# Data Manager
DataManager <- R6::R6Class(
  "DataManager",
  inherit = cecelia::ReactiveObject,
  
  ### private
  private = list(
    cciaImageCollectionUID = CCID_IMAGE_COLLECTION,
    # cciaAnalysisCollectionUID = CCID_ANALYSIS_COLLECTION,
    handleCciaImageCollection = NULL,
    # handleCciaAnalysisCollection = NULL,
    
    # flag whether data manager is initialised
    handleInitialised = FALSE,
    
    # setters
    setCciaImageCollection = function(x, invalidate = TRUE) {
      private$handleCciaImageCollection <- x
      private$invalidate(invalidate = invalidate)
    },
    
    # setCciaAnalysisCollection = function(x, invalidate = TRUE) {
    #   private$handleCciaAnalysisCollection <- x
    #   private$invalidate(invalidate = invalidate)
    # },
    
    setInitialised = function(x, invalidate = TRUE) {
      private$handleInitialised <- x
      private$invalidate(invalidate = invalidate)
    },
    
    # getters
    initialised = function() {
      private$handleInitialised
    }
  ),
  
  ### public
  public = list(
    # init collections
    initCcciaCollections = function(objDir, reset = FALSE) {
      if (private$initialised() == FALSE || reset == TRUE) {
        # set flag
        private$setInitialised(TRUE, invalidate = FALSE)
        
        curParamsImage <- list(
          Name = "Root",
          Type = "Image",
          Class = "CciaImageCollection"
        )
        
        # curParamsAnalysis <- list(
        #   Name = "Root",
        #   Type = "Analysis",
        #   Class = "CciaAnalysisCollection"
        # )
        
        # init image and analysis collections
        # collections should load all their dependencies
        # during initialize
        private$setCciaImageCollection(
          CciaImageCollection$new(
            file.path(
              objDir,
              private$cciaImageCollectionUID,
              CCID_STATE_FILE),
            private$cciaImageCollectionUID,
            initParams = curParamsImage
          )$reactive()
        )
        
        # private$setCciaAnalysisCollection(
        #   CciaAnalysisCollection$new(
        #     file.path(
        #       objDir,
        #       private$cciaAnalysisCollectionUID,
        #       CCID_STATE_FILE),
        #     private$cciaAnalysisCollectionUID,
        #     initParams = curParamsAnalysis
        #   )$reactive()
        # )
      }
    },
    
    # save collections
    saveCciaCollections = function() {
      saveStates <- TRUE
      
      if (!is.null(self$cciaImageCollection())){
        self$cciaImageCollection()$saveState()
      }
      
      # if (!is.null(self$cciaAnalysisCollection())){
      #   self$cciaAnalysisCollection()$saveState()
      # }
    },
    
    cciaImageCollection = function() {
      retVal <- NULL
      
      if (!is.null(private$handleCciaImageCollection)){
        retVal <- private$handleCciaImageCollection()
      }
      
      retVal
    }
    
    # cciaAnalysisCollection = function() {
    #   retVal <- NULL
    #   
    #   if (!is.null(private$handleCciaAnalysisCollection)){
    #     retVal <- private$handleCciaAnalysisCollection()
    #   }
    #   
    #   retVal
    # }
    
    # setters
    
    # getters
  )
)
