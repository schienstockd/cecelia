#' Viewer manager
#' 
#' @name ViewerManager
#' @description Viewer manager
#'
#' @examples
#' TODO
#' @export
ViewerManager <- R6::R6Class(
  "ViewerManager",
  inherit = cecelia::ReactiveObject,
  
  ### private
  private = list(
    handleViewer = NULL,
    lazyLoading = FALSE,
    handleImObj = NULL,
    handleViewerOutput = NULL,
    handleViewerInput = NULL,
    shownUID = "",
    ignoreCalls = FALSE,
    
    # viewer settings
    useChannelAxis = TRUE,
    showViewer = TRUE,
    showOriginal = FALSE,
    layersVisible = FALSE,
    show3D = FALSE,
    showLabels = FALSE,
    showPoints = FALSE,
    showPops = FALSE,
    showNeighbours = FALSE,
    showShapes = FALSE,
    showTracks = FALSE,
    asDask = TRUE,
    multiscales = TRUE,
    
    # setters
    setViewer = function(x, invalidate = TRUE) {
      private$handleViewer = x
      private$invalidate(invalidate = invalidate)
    },
    
    setImObj = function(x, invalidate = TRUE) {
      private$handleImObj = x
      private$invalidate(invalidate = invalidate)
    },
    
    setShownUID = function(x, invalidate = TRUE) {
      private$shownUID = x
      private$invalidate(invalidate = invalidate)
    },
    
    # getters
    getViewer = function() {
      private$handleViewer
    },
    
    getShownUID = function() {
      private$shownUID
    }
  ),
  
  ### public
  public = list(
    ## init
    initialize = function(session) {
      super$initialize(session)
    },
    
    # return viewer
    viewer = function(
      viewerOutputFile = file.path(
        cciaConf()$python$viewer$viewerPath,
        cciaConf()$python$viewer$outputFile),
      viewerInputFile = file.path(
        cciaConf()$python$viewer$viewerPath,
        cciaConf()$python$viewer$inputFile)
      ) {
      
      if (is.null(private$getViewer()) && self$getIgnoreCalls() == FALSE && DEBUG_SHOW_VIEWER == TRUE) {
        # init viewer
        # viewer <- NapariUtils$new()
        viewer <- cciaNapariUtils()
        viewer$initNapari()
        
        private$setViewer(viewer)
      }
      
      private$getViewer()
    },
    
    # add pixel classification pane
    addPixclPane = function() {
      if (!is.null(self$viewer()))
        self$viewer()$addPixclPane()
    },
    
    # add animation pane
    addAnimationPane = function() {
      if (!is.null(self$viewer()))
        self$viewer()$addAnimationPane()
    },
    
    # open image
    openImage = function(imObj, napariModule,
                         showLabelsAsNpArray = FALSE,
                         forceReloadData = FALSE) {
      # if (!is.null(imObj()$imFilepath()) &&
      if (length(imObj()$imFilepath()) > 0 &&
        file.exists(imObj()$imFilepath())) {
        # reset data from currently shown image
        if (length(private$handleImObj) > 0)
          private$handleImObj()$resetData()
        
        # retrieve data for current image
        imObj()$loadData(forceReloadData)
        
        # clear input
        self$updateInput(NULL)
        self$updateOutput(NULL)
        
        # get filepath
        if (self$getShowOriginal() == TRUE) {
          imFilepath <- imObj()$imFilepath(valueName = "default")
          imChannelNames <- imObj()$imChannelNames(valueName = "default")
        } else {
          imFilepath <- imObj()$imFilepath()
          imChannelNames <- imObj()$imChannelNames()
        }
        
        # show image in napari if not already shown
        if (!is.null(self$viewer()) && (
          is.null(self$shownImage()) || private$getShownUID() != imObj()$getUID()
        )) {
          self$viewer()$openImage(
            imFilepath,
            useChannelAxis = self$getUseChannelAxis(),
            imChannelNames = imChannelNames,
            napariModule = napariModule,
            asDask = self$getAsDask(),
            multiscales = self$getMultiscales(),
            show3D = self$getShow3D(),
            layersVisible = self$getLayersVisible()
          )
          
          # set uID
          private$setShownUID(imObj()$getUID())
        }
        
        # go through value names and show
        # TODO there should be a selection box
        # somewhere to only show selected pops
        valueNames <- imObj()$valueNames("imLabelsFilepath")
        
        # get label suffixes
        labelSuffixes <- lapply(
          as.list(valueNames), function(x) as.list(imObj()$valueSuffixes(
            "imLabelsFilepath", valueName = x))
        )
        labelSuffixes <- labelSuffixes[lengths(labelSuffixes) > 0]
        
        if (length(valueNames) > 0 && !is.null(self$viewer())) {
          # call viewer
          self$viewer()$showLabelsAll(
            valueNames = valueNames,
            showLabels = self$getShowLabels(),
            showPoints = self$getShowPoints(),
            showTracks = self$getShowTracks(),
            asNpArray = showLabelsAsNpArray,
            labelSuffixes = labelSuffixes
          )
        }
        
        # set current image shown
        private$setImObj(imObj)
      }
    },
    
    # return currently shown image
    shownImage = function() {
      retVal <- NULL
      
      if (!is.null(private$handleImObj)) {
        retVal <- private$handleImObj
      }
      
      retVal
    },
    
    # return output
    viewerOutput = function() {
      private$handleViewerOutput
    },
    
    # return input
    viewerInput = function() {
      private$handleViewerInput
    },
    
    # clear viewer output
    clearViewerOutput = function() {
      self$updateOutput(NULL)
      
      # clear viewer
      if (!is.null(self$viewer()))
        self$viewer()$clearViewerOutput()
    },
    
    # clear viewer input
    clearViewerInput = function() {
      self$updateInput(NULL)
      
      # clear viewer
      if (!is.null(self$viewer()))
        self$viewer()$clearViewerInput()
    },
    
    # update output from file
    updateOutput = function(viewerOutput, invalidate = TRUE) {
      private$handleViewerOutput <- viewerOutput
      private$invalidate(invalidate = invalidate)
    },
    
    # update input from file
    updateInput = function(viewerInput, invalidate = TRUE) {
      private$handleViewerInput <- viewerInput
      private$invalidate(invalidate = invalidate)
    },
    
    # close viewer
    closeViewer = function() {
      if (!is.null(private$getViewer())){
        self$viewer()$closeViewer()
        
        # reset viewer
        private$setViewer(NULL)
      }
    },
    
    # quit
    quit = function(quitKernelProcess = TRUE) {
      if (!is.null(private$getViewer())){
        # quit viewer
        self$viewer()$quitKernel()
        
        # reset viewer
        private$setViewer(NULL)
      }
      
      # # quit kernel process
      # if (quitKernelProcess == TRUE)
      #   cciaNapariUtils$quitKernel()
    },
    
    # setters
    setLazyLoading = function(x, invalidate = TRUE) {
      private$lazyLoading <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setUseChannelAxis = function(x, invalidate = TRUE) {
      private$useChannelAxis <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShowViewer = function(x, invalidate = TRUE) {
      private$showViewer <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShowOriginal = function(x, invalidate = TRUE) {
      private$showOriginal <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setLayersVisible = function(x, invalidate = TRUE) {
      private$layersVisible <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShow3D = function(x, invalidate = TRUE) {
      private$show3D <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShowLabels = function(x, invalidate = TRUE) {
      private$showLabels <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShowPoints = function(x, invalidate = TRUE) {
      private$showPoints <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShowTracks = function(x, invalidate = TRUE) {
      private$showTracks <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setAsDask = function(x, invalidate = TRUE) {
      private$asDask <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShowPops = function(x, invalidate = TRUE) {
      private$showPops <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShowNeighbours = function(x, invalidate = TRUE) {
      private$showNeighbours <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setShowShapes = function(x, invalidate = TRUE) {
      private$showShapes <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setMultiscales = function(x, invalidate = TRUE) {
      private$multiscales <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setIgnoreCalls = function(x, invalidate = TRUE) {
      private$ignoreCalls <- x
      private$invalidate(invalidate = invalidate)
    },
    
    # getters
    getLazyLoading = function() {
      private$lazyLoading
    },
    
    getUseChannelAxis = function() {
      private$useChannelAxis
    },
    
    getShowViewer = function() {
      private$showViewer
    },
    
    getShowOriginal = function() {
      private$showOriginal
    },
    
    getLayersVisible = function() {
      private$layersVisible
    },
    
    getShow3D = function() {
      private$show3D
    },
    
    getShowLabels = function() {
      private$showLabels
    },
    
    getShowPoints = function() {
      private$showPoints
    },
    
    getShowTracks = function() {
      private$showTracks
    },
    
    getAsDask = function() {
      private$asDask
    },
    
    getShowPops = function() {
      private$showPops
    },
    
    getShowNeighbours = function() {
      private$showNeighbours
    },
    
    getShowShapes = function() {
      private$showShapes
    },
    
    getMultiscales = function() {
      private$multiscales
    },
    
    getIgnoreCalls = function() {
      private$ignoreCalls
    }
  )
)
