FlowGatingPlot <- R6::R6Class(
  "FlowGatingPlot",
  inherit = cecelia::ReactiveObject,
  
  ## private
  private = list(
    # plot conf
    plotName = NULL,
    plotXchannel = NULL,
    plotYchannel = NULL,
    plotXchannelScale = NULL,
    plotYchannelScale = NULL,
    plotXchannelLimit = NULL,
    plotYchannelLimit = NULL,
    plotPopPath = NULL,
    plotPopLeaves = c(),
    plotType = NULL,
    
    # filters
    plotFilterMeasure = "NONE",
    plotFilterValues = NULL,
    plotFilterLabels = NULL,
    
    # plot params
    boxIDs = NULL
    
    # setters
    
    # getters
  ),
  
  ### public
  public = list(
    # plot reactives
    plotReactiveSelected = NULL,
    plotReactiveAfterplot = NULL,
    plotReactiveRelayout = NULL,
    plotReactiveHover = NULL,
    
    initialize = function(boxIDs) {
      # set box ids
      self$setBoxIDs(boxIDs)
      
      # get selected for plot
      self$plotReactiveSelected <- reactive({
        suppressWarnings(
          event_data("plotly_selected", self$getBoxIDs()$plot)
        )
      })
      
      # get afterplot events for plot
      self$plotReactiveAfterplot <- reactive({
        suppressWarnings(
          event_data("plotly_afterplot", self$getBoxIDs()$plot)
        )
      })
      
      # get relayout events for plot
      self$plotReactiveRelayout <- reactive({
        suppressWarnings(
          event_data("plotly_relayout", self$getBoxIDs()$plot)
        )
      })
      
      # get hover events for plot
      self$plotReactiveHover <- reactive({
        suppressWarnings(
          event_data("plotly_hover", self$getBoxIDs()$plot)
        )
      })
    },
    
    # setters
    setPlotName = function(x, invalidate = TRUE) {
      private$plotName <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotXchannel = function(x, invalidate = TRUE) {
      private$plotXchannel <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotYchannel = function(x, invalidate = TRUE) {
      private$plotYchannel <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotXchannelScale = function(x, invalidate = TRUE) {
      private$plotXchannelScale <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotYchannelScale = function(x, invalidate = TRUE) {
      private$plotYchannelScale <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotXchannelLimit = function(x, invalidate = TRUE) {
      private$plotXchannelLimit <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotYchannelLimit = function(x, invalidate = TRUE) {
      private$plotYchannelLimit <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotPopPath = function(x, invalidate = TRUE) {
      private$plotPopPath <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotPopLeaves = function(x, invalidate = TRUE) {
      private$plotPopLeaves <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotType = function(x, invalidate = TRUE) {
      private$plotType <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotFilterMeasure = function(x, invalidate = TRUE) {
      private$plotFilterMeasure <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotFilterValues = function(x, invalidate = TRUE) {
      private$plotFilterValues <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setPlotFilterLabels = function(x, invalidate = TRUE) {
      private$plotFilterLabels <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setBoxIDs = function(x, invalidate = TRUE) {
      private$boxIDs <- x
      private$invalidate(invalidate = invalidate)
    },
    
    # getters
    getPlotName = function() {
      private$plotName
    },
    
    getPlotXchannel = function(flowName = FALSE) {
      retVal <- private$plotXchannel
      
      if (flowName == TRUE)
        retVal <- flowCorrectChannelNames(retVal)
      
      retVal
    },
    
    getPlotYchannel = function(flowName = FALSE) {
      retVal <- private$plotYchannel
      
      if (flowName == TRUE)
        retVal <- flowCorrectChannelNames(retVal)
      
      retVal
    },
    
    getPlotChannels = function(flowName = FALSE) {
      c(
        self$getPlotXchannel(flowName = flowName),
        self$getPlotYchannel(flowName = flowName)
      )
    },
    
    getPlotXchannelScale = function() {
      retVal <- private$plotXchannelScale
      
      retVal
    },
    
    getPlotYchannelScale = function() {
      retVal <- private$plotYchannelScale
      
      retVal
    },
    
    getPlotYchannelLimit = function() {
      private$plotPopPath
    },
    
    getPlotPopPath = function(normaliseRoot = FALSE, unlist = FALSE) {
      path <- private$plotPopPath
      
      # normalise root to '/'
      if (normaliseRoot == TRUE) {
        path <- flowNormRootPath(path)
      }
      
      # unlist before returning?
      if (unlist == TRUE) {
        path <- c(path, use.names = FALSE)
      }
        
      path
    },
    
    getPlotPopLeaves = function(unlist = FALSE) {
      leaves <- private$plotPopLeaves
      
      # unlist before returning?
      if (unlist == TRUE) {
        leaves <- c(leaves, use.names = FALSE)
      }
      
      leaves
    },
    
    getPlotType = function() {
      private$plotType
    },
    
    getPlotFilterMeasure = function() {
      private$plotFilterMeasure
    },
    
    getPlotFilterValues = function() {
      private$plotFilterValues
    },
    
    getPlotFilterLabels = function() {
      private$plotFilterLabels
    },
    
    getBoxIDs = function() {
      private$boxIDs
    }
  )
)
