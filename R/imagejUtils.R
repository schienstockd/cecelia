#' Utils to call ImageJ
#' 
#' @name ImagejUtils
#' @description Utils to call ImageJ
#'
#' @examples
#' TODO
#' @export
ImagejUtils <- R6::R6Class(
  "ImagejUtils",
  inherit = R6P::Singleton,
  
  ## public
  public = list(
    #' @description Init params
    #' @param fijiPath character for file path to ImageJ
    #' @param scriptsPath character for file path to ImageJ scripts/macros
    initParams = function(fijiPath, scriptsPath) {
      private$setFijiPath(fijiPath)
      private$setScriptsPath(scriptsPath)
    },
    
    #' @description Run donblo segmentation
    #' @param imPath character for file path to image
    #' @param blobChannels list of numeric for blob channels
    #' @param donutChannels list of numeric for donut channels
    #' @param cellRadius numeric for cell radius
    #' @param gaussianFilter integer for gaussian filter
    #' @param medianFilter integer for median filter
    #' @param minimumFilter integer for minimum filter
    #' @param maximumFilter integer for maximum filter
    #' @param detectionThreshAdj numeric for threshold adjustment
    #' @param filteringAfterSeg integer for post segmentation filter
    #' @param rollingRadius integer for rolling ball filter
    #' @param segPath character for file path to save segmentation
    runDonblo = function(
      imPath, blobChannels = list(), donutChannels = list(), cellRadius = 3.0,
      gaussianFilter = 1L, medianFilter = 1L, minimumFilter = 1L, maximumFilter = 1L,
      detectionThreshAdj = 0.4, filteringAfterSeg = 0L, rollingRadius = 10L, segPath = "") {
      
      # map parameters
      scriptParams <- list(
        imPath = imPath,
        segPath = segPath,
        cellRadius = cellRadius,
        blobChannels = blobChannels,
        donutChannels = donutChannels,
        gaussianFilter = gaussianFilter,
        medianFilter = medianFilter,
        maximumFilter = maximumFilter,
        minimumFilter = minimumFilter,
        detectionThreshAdj = detectionThreshAdj,
        filteringAfterSeg = filteringAfterSeg,
        rollingRadius = rollingRadius
      )

      # run command
      self$runJython("runDonblo", scriptParams)
    },
    
    #' @description Create image from sequence
    #' @param imPathIn character for file path to image
    #' @param imPathOut character for file path to out
    #' @param switchZforC integer (0,1) to switch 'C' and 'Z'
    createFromSequence = function(imPathIn, imPathOut, switchZforC = 0) {
      
      # map parameters
      scriptParams <- list(
        imPathIn = imPathIn,
        imPathOut = imPathOut,
        switchZforC = switchZforC
      )

      # run command
      self$runJython("createFromSequence", scriptParams)
    },
    
    #' @description Export image
    #' @param imPathIn character for file path to image
    #' @param imPathOut character for file path to out
    exportImage = function(imPathIn, imPathOut) {
      
      # map parameters
      scriptParams <- list(
        imPathIn = imPathIn,
        imPathOut = imPathOut
      )

      # run command
      self$runJython("exportImage", scriptParams)
    },
    
    #' @description Prepare parameters for jython
    #' @param scriptParams list of list of characters for parameters
    prepJythonParams = function(scriptParams) {
      # add '"' if it is a string
      for (curParam in names(scriptParams)) {
        if (is.character(scriptParams[[curParam]])) {
          scriptParams[[curParam]] <- sprintf(
            '"%s"', scriptParams[[curParam]])
        }
      }
      
      scriptParams <- lapply(lapply(scriptParams, reticulate::r_to_py), py_str)
      
      # https://imagej.github.io/scripting/headless
      scriptParamsString <- paste0(
        "'",
        paste(sprintf(
          '%s = %s',
          paste(names(scriptParams)),
          paste(scriptParams)
        ), collapse = ", "),
        "'"
      )
      
      scriptParamsString
    },
    
    #' @description Run script with parameters
    #' @param scriptName character for script name
    #' @param scriptParams list of list of characters for parameters
    runJython = function(scriptName, scriptParams) {
      # prep params
      paramString <- self$prepJythonParams(scriptParams)
      
      print(paramString)
      
      # prepare command
      cmd <- paste(
        private$getFijiPath(),
        "--ij2", "--headless", "--console", "--run",
        file.path(private$getScriptsPath(), paste0(scriptName, ".py")),
        paramString
      )
      
      # go to module path
      cmd <- paste(
        sprintf("cd %s", private$getScriptsPath()),
        cmd, sep = ";"
      )
      
      # call fiji
      handleSystem(.execSystem(cmd))
    }
  ),
  
  ## private
  private = list(
    fijiPath = NULL,
    scriptsPath = NULL,
    
    ## setters
    setFijiPath = function(x) {
      private$fijiPath <- x
    },
    
    setScriptsPath = function(x) {
      private$scriptsPath <- file.path(getwd(), x)
    },
    
    ## getters
    getFijiPath = function() {
      private$fijiPath
    },
    
    getScriptsPath = function() {
      private$scriptsPath
    }
  )
)
