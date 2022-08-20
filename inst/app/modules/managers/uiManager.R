# Generate and manage UI elements for main module
UIManager <- R6::R6Class(
  "UIManager",
  private = list(
    session = NULL,
    input = NULL,

    ## plotly helpers
    # change grid colour
    plotlyAx = list(
      type = "linear",
      zeroline = TRUE,
      showline = TRUE,
      gridwidth = 1,
      zerolinewidth = 2,
      linewidth = 1,
      gridcolor = "#222",
      zerolinecolor = "#999",
      # linecolor = "#999",
      title = list(text = ""),
      tickfont = list(color = "#d3d3d3")
      # ticks = "",
      # showticklabels = FALSE
    ),

    # remove mode buttons
    plotlyModeBarButtonsToRemove = list(
      # "select2d", "lasso2d",
      "toggleSpikelines",
      "hoverClosestGl2d", "hoverClosestPie",
      "hoverClosestCartesian", "hoverCompareCartesian"
    ),

    # setters
    setInput = function(x) {
      private$input <- x
    },

    setSession = function(x) {
      private$session <- x
    },

    # getters
    getInput = function() {
      private$input
    },

    getSession = function() {
      private$session
    }
  ),

  public = list(
    # init
    initialize = function(input, session) {
      private$setInput(input)
      private$setSession(session)
    },

    # generate colours for flow plots
    flowColours = function(x, y, nbin = 128) {
      flowColours(x, y, nbin = nbin)
    },

    # return layout for flow plots
    flowPlot_lyLayout = function(p1, ...)  {
      layoutList <- self$flowPlot_lyLayoutList(...)
      layoutList$p <- p1

      # set plot
      do.call(layout, layoutList)
    },

    # return layout list
    flowPlot_lyLayoutList = function(ax = NULL, ay = NULL, xlab = NULL, ylab = NULL,
                                     xlim = NULL, ylim = NULL, enableGating = TRUE,
                                     axisScale = NULL, reverseRange = NULL,
                                     xScale = "linear", yScale = "linear") {
      # set axis if not set
      if (is.null(ax)) ax <- private$plotlyAx
      if (is.null(ay)) ay <- private$plotlyAx

      # set scale
      if (!is.null(xScale)) ax$type = xScale
      if (!is.null(yScale)) ay$type = yScale

      # set titles
      if (!is.null(xlab)) ax$title = xlab
      if (!is.null(ylab)) ay$title = ylab

      # set range
      if (!is.null(xlim)) ax$range = xlim
      if (!is.null(ylim)) ay$range = ylim

      # reverse range
      if (!is.null(reverseRange)) {
        if ("x" %in% reverseRange)
          ax$autorange <- "reversed"
        if ("y" %in% reverseRange)
          ay$autorange <- "reversed"
      }

      # set ratio
      if (!is.null(axisScale)) {
        for (i in names(axisScale)) {
          x <- axisScale[[i]]

          if (i == "x") {
            ax$scaleanchor = x$anchor
            ax$scaleratio = x$ratio
          }

          if (i == "y") {
            ay$scaleanchor = x$anchor
            ay$scaleratio = x$ratio
          }
        }
      }

      list(
        plot_bgcolor = "#222",
        paper_bgcolor = "#222",
        font = list(color = 'white'),
        showlegend = FALSE,
        xaxis = ax,
        yaxis = ay,
        # dragmode = "pan",
        # dragmode = if (enableGating == TRUE) "drawclosedpath" else "select"
        dragmode = if (enableGating == TRUE) "drawrect" else "select"
        # dragmode = "drawclosedpath"
      )
    },

    # get a flow plot
    flowPlot_ly = function(id, enableGating = TRUE, screenshotConf = NULL) {
      p1 <- plotly::plot_ly(source = id)

      if (enableGating == TRUE) {
        # set screenshot dimensions
        if (!is.null(screenshotConf)) {
          screenshotHeight <- screenshotConf$height
          screenshotWidth <- screenshotConf$width
        } else {
          screenshotHeight <- cciaConf()$fcs$gating$screenshot$height
          screenshotWidth <- cciaConf()$fcs$gating$screenshot$width
        }

        p1 <- p1 %>%
        config(
          modeBarButtonsToAdd = list(
            "drawrect", "drawclosedpath"
          ),
          toImageButtonOptions = list(
            format = cciaConf()$fcs$gating$screenshot$format,
            height = screenshotHeight,
            width = screenshotWidth
          )
        )
      }

      p1
    },

    # get point trace for flow plot
    # https://rglab.github.io/ggcyto/articles/advanced/ggplot.flowSet.2d.html
    flowPlot_lyPointTraceList = function(
      dt, x, y, markerOpacity = 1, markerSize = cciaConf()$fcs$gating$default$markerSize,
      colours = NULL, coloursOrder = NULL, flowNames = TRUE, customdata = NULL,
      useFlowColours = TRUE, expandData = FALSE, plotType = "pseudocolour") {
      # convert channel names
      flowX <- x
      flowY <- y

      if (flowNames == TRUE) {
        flowX <- .flowCorrectChannelNames(flowX)
        flowY <- .flowCorrectChannelNames(flowY)
      }

      # get colours
      if (plotType == "pseudocolour") {
        if (is.null(colours) && useFlowColours == TRUE) {
          colours <- self$flowColours(
            dt[[flowX]], dt[[flowY]])
        }
      } else if (plotType == "contour") {
        # TODO set custom data to null
        # you will have to filter the values
        customdata <- NULL
      }

      traces <- list()
      if (!is.null(coloursOrder)) {
        # go through colours
        for (x in coloursOrder) {
          curDT <- dt[colours == x,]

          if (!is.null(customdata))
            curCD <- customdata[colours == x]
          else
            curCD <- NULL

          traces[[x]] <- list(
            # data = curDT,
            # x = ~get(flowX),
            # y = ~get(flowY),
            customdata = curCD,
            marker = list(
              color = x,
              size = markerSize,
              opacity = markerOpacity
            ),
            hoverinfo = "skip",
            type = "scattergl",
            mode = "markers"
          )

          # add contour
          if (plotType == "contour") {
            # get contour lines
            contourDT <- flowContourLines(
              curDT, flowX, flowY,
              n = cciaConf()$fcs$gating$contour$resolution,
              confidenceLevels = cciaConf()$fcs$gating$contour$levels
              )

            # TODO outliers will not be shown
            traces[[paste(x, "contour", sep = "_")]] <- list(
            # traces[[x]] <- list(
                data = contourDT,
                x = ~x,
                y = ~y,
                hoverinfo = "skip",
                type = "scattergl",
                mode = "lines",
                line = list(
                  color = x,
                  width = 1
                )
              )
          }

          # expand data or use data table?
          # this is called when changing traces
          if (expandData == TRUE) {
            # need to update DT if using contours
            if (plotType == "contour") {
              traces[[paste(x, "contour", sep = "_")]]$x <- contourDT$x
              traces[[paste(x, "contour", sep = "_")]]$y <- contourDT$y
              # https://plotly.com/r/group-by/
              traces[[paste(x, "contour", sep = "_")]]$transforms <- list(
                list(
                  type = 'groupby',
                  groups = contourDT$seq
                ))
              
              traces[[x]]$x <- curDT[in_contour == 0,][[flowX]]
              traces[[x]]$y <- curDT[in_contour == 0,][[flowY]]
            } else {
              traces[[x]]$x <- curDT[[flowX]]
              traces[[x]]$y <- curDT[[flowY]]
            }
          } else {
            # TODO does not seem to work for subpopulations
            if (plotType == "contour")
              traces[[x]]$data <- curDT[in_contour == 0,]
            else
              traces[[x]]$data <- curDT
            
            traces[[x]]$x <- ~get(flowX)
            traces[[x]]$y <- ~get(flowY)
          }
        }
      } else {
        # reset colour
        if (plotType == "contour" && useFlowColours == TRUE)
          colours <- NULL

        traces <- list(list(
          # data = dt,
          # x = ~get(flowX),
          # y = ~get(flowY),
          customdata = customdata,
          marker = list(
            color = if (!is.null(colours)) colours else "grey",
            size = markerSize,
            opacity = markerOpacity
          ),
          hoverinfo = "skip",
          type = "scattergl",
          mode = "markers"
        ))

        # add contour
        if (plotType == "contour") {
          # get contour lines
          # TODO could you use data.table grouping?
          contourDT <- flowContourLines(
            dt, flowX, flowY,
            n = cciaConf()$fcs$gating$contour$resolution,
            confidenceLevels = cciaConf()$fcs$gating$contour$levels
            )

          traces <- append(
            traces,
            list(list(
              data = contourDT,
              x = ~x,
              y = ~y,
              hoverinfo = "skip",
              type = "scattergl",
              mode = "lines",
              line = list(
                color = if (!is.null(colours)) colours else "white",
                width = 1
              )
            ))
          )
        }

        # expand data or use data table?
        # this is called when changing traces
        if (expandData == TRUE) {
          # need to update DT if using contours
          if (plotType == "contour") {
            traces[[2]]$x <- contourDT$x
            traces[[2]]$y <- contourDT$y
            # https://plotly.com/r/group-by/
            traces[[2]]$transforms <- list(
              list(
                type = 'groupby',
                groups = contourDT$seq
              ))
            
            traces[[1]]$x <- dt[in_contour == 0,][[flowX]]
            traces[[1]]$y <- dt[in_contour == 0,][[flowY]]
          } else {
            traces[[1]]$x <- dt[[flowX]]
            traces[[1]]$y <- dt[[flowY]]
          }
        } else {
          # TODO does not seem to work for subpopulations
          if (plotType == "contour")
            traces[[1]]$data <- dt[in_contour == 0,]
          else
            traces[[1]]$data <- dt
          
          traces[[1]]$x <- ~get(flowX)
          traces[[1]]$y <- ~get(flowY)
        }
      }

      # return list
      traces
    },

    # add traces to flow plot
    flowPlot_lyTraces = function(p1, ...) {
      # get point trace
      pointTraces <- self$flowPlot_lyPointTraceList(...)

      # go through traces
      for (x in pointTraces) {
        # set plot
        x$p <- p1

        # add to plot
        p1 <- do.call(add_trace, x)
      }

      p1
    },

    # clear traces for plot_ly
    flowPlot_lyRemoveTraces = function(id, deferUntilFlush = TRUE, traceIDs = c(0), n = 0) {
      p1 <- plotly::plotlyProxy(id, private$getSession(), deferUntilFlush = deferUntilFlush)

      # go through traces
      if (n > 0) {
        for (i in seq(n)) {
          p1 %>% plotlyProxyInvoke("deleteTraces", 0)
        }
      } else if (length(traceIDs) > 0) {
        p1 %>% plotlyProxyInvoke("deleteTraces", traceIDs)
      }
    },

    # change traces for plot_ly
    flowPlot_lyChangeTrace = function(id, dt, x, y,
                                      deferUntilFlush = TRUE, removeTraces = TRUE,
                                      markerOpacity = 1, markerSize = cciaConf()$fcs$gating$default$markerSize,
                                      colours = NULL, coloursOrder = NULL, enableGating = TRUE,
                                      customdata = NULL, useFlowColours = TRUE, reverseRange = NULL,
                                      plotType = "pseudocolour", ...) {
      # prepare layout
      layoutList <- self$flowPlot_lyLayoutList(
        xlab = x, ylab = y, ...)

      # remove traces
      if (removeTraces == TRUE) {
        flowPlot_lyRemoveTraces(id, deferUntilFlush = deferUntilFlush)
      }

      # prepare traces
      pointTraces <- self$flowPlot_lyPointTraceList(
        dt, x, y, markerOpacity = markerOpacity, markerSize = markerSize,
        colours = colours, coloursOrder = coloursOrder, customdata = customdata,
        useFlowColours = useFlowColours, expandData = TRUE, plotType = plotType)

      # go through traces
      p1 <- plotly::plotlyProxy(id, private$getSession(), deferUntilFlush = deferUntilFlush)
      
      for (x in pointTraces) {
        p1 %>% plotlyProxyInvoke("addTraces", x)
      }
    },

    # get shape counts from relayout
    pot_lyRelayoutCounts = function(relayoutEvents, trimNames = TRUE) {
      relayoutCounts <- unlist(lapply(relayoutEvents, function(x) lapply(x, nrow)))

      # trim away
      if (!is.null(relayoutCounts) && trimNames == TRUE) {
        names(relayoutCounts) <- stringr::str_extract(names(relayoutCounts), ".*(?=\\.)")
      }

      relayoutCounts
    },

    # add shapes to plot
    plot_lyAddShapesWithAnnotations = function(
      id, shapes, deferUntilFlush = TRUE, annotations = list()) {
      plotly::plotlyProxy(id, private$getSession(), deferUntilFlush = deferUntilFlush) %>%
        # add gates
        plotlyProxyInvoke("relayout", shapes = shapes, annotations = annotations)
    },

    # clear shapes form plot
    plot_lyClearShapesWithAnnotations = function(id, deferUntilFlush = TRUE) {
      plotly::plotlyProxy(id, private$getSession(), deferUntilFlush = deferUntilFlush) %>%
        # remove shapes
        plotlyProxyInvoke("relayout", shapes = list(), annotations = list())
    },

    # generate data table
    dataTable = function(datCols, options = NULL, rownames = FALSE,
                         editable = FALSE, ordering = FALSE, pageLength = -1,
                         dom = 'ti') {

      # set default options
      # https://datatables.net/reference/option/dom
      defaultOptions <- list(
        dom = dom, ordering = ordering, lengthChange = if (pageLength > 0) FALSE else TRUE,
        scrollX = TRUE, scrollY = TRUE, scrollCollapse = TRUE,
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        pageLength = pageLength
      )

      # add options
      if (!is.null(options)) {
        defaultOptions <- append(defaultOptions, options)
      }

      datatable(
        if (is.list(datCols)) do.call(cbind, datCols) else datCols,
        options = defaultOptions,
        rownames = rownames, escape = FALSE, selection = 'none',
        editable = editable)
    }
  )
)
