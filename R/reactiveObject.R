#' Adapted from
#' https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class/84890/8
#' 
#' @name ReactiveObject
#' @description Shiny reactive object
#' @importFrom shiny reactive reactiveVal
#'
#' @examples
#' ## init reactive object
#' reactObj <- ReactiveObject$new()
#' reactObj$reactive()
#' reactObj() # will react in context
#' ## init not-reactive object
#' reactObj <- ReactiveObject$new(initReactivity = FALSE)
#' reactObj # normal/non-reactive object
#' @export
ReactiveObject <- R6::R6Class("ReactiveObject",
  private = list(
    reactiveDep = NULL,
    reactiveExpr = NULL,
    
    # for any inheriting classes to know
    # whether to init objects w/o reactivity
    handleInitReactivity = TRUE,
    
    #' @description
    #' Invalidate object; not invalidating the object cause shiny
    #' not to react to changes on the object. This is a workaround
    #' as the individual attributes of the object are not reactive
    #' themselves.
    #' @param invalidate Invalidate or not
    invalidate = function(invalidate = TRUE) {
      # only invalidate if this object is reactive
      # then this can also be used in non-reactive context
      if (invalidate == TRUE && private$isReactive() == TRUE) {
        private$count <- private$count + 1
        private$reactiveDep(private$count)
      }
      
      invisible()
    },
    
    count = 0,
    session = NULL,
    
    #' @description Check if the object is reactive.
    isReactive = function() {
      reactive <- FALSE
      
      if ("reactiveDep" %in% names(private) &&
          !is.null(private$reactiveDep) &&
          !is.null(private$reactiveDep())) {
        reactive <- TRUE
      }
      
      reactive
    },
    
    ## setters
    setInitReactivity = function(x) {
      private$handleInitReactivity <- x
    },
    
    ## getters
    initReactivity = function() {
      private$handleInitReactivity
    }
  ),
  public = list(
    #' @description Init class
    #' @param session Set current session of shiny context
    #' @param initReactivity Boolean whether object is reactive or not
    initialize = function(session = NULL, initReactivity = TRUE) {
      # Until someone calls $reactive(), private$reactiveDep() is a no-op. Need
      # to set it here because if it's set in the definition of private above, it will
      # be locked and can't be changed.
      private$reactiveDep <- function(x) NULL
      
      # set reactivity flag
      private$setInitReactivity(initReactivity)
      
      # set session
      if (!is.null(session)) {
        private$session <- session
      }
    },
    
    #' @description Init reactivity of object
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- reactiveVal(0)
        private$reactiveExpr <- reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    },
    
    #' @description
    #' In the rare case you want to trigger invalidate
    #' from outside the object. This should ideally NOT be used.
    update = function() {
      private$invalidate()
    }
  )
)