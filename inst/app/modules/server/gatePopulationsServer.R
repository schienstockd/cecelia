#' @description Server to gate populations
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.gatePopulationsServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Functions
      # reset object information from gating set
      resetObjInfoFromGatingSet <- function(
        popParent = "root", includeParent = FALSE, parentPath = NULL) {
        # get populations
        pops <- cciaObj()$flowGatingSet()$popLeaves(popParent)
        
        # was the parent population renamed before?
        if (is.null(parentPath)) parentPath <- popParent
        
        # include parent
        if (!cciaObj()$flowGatingSet()$popIsRoot(popParent)) {
          if (includeParent == TRUE) {
            pops <- c(pops, parentPath)
          }
        }
        
        # update objects
        for (x in pops) {
          pathForID <- x
          
          # is x a leave of the renamed population?
          if (!cciaObj()$flowGatingSet()$popIsRoot(popParent)) {
            if (!is.na(stringr::str_match(x, popParent)[[1]])) {
              pathForID <- cciaObj()$flowGatingSet()$changeParentName(x, parentPath)
            }
          }
          
          # get ID from path
          popID <- cciaObj()$popIDByAttr(
            popType(), "path", pathForID)
          
          # adjust for parent
          if (x == parentPath){
            x <- popParent
          } 
          
          # get gate IDs
          plotGateID <- cciaObj()$flowGatingSet()$popGateID(x)
          
          # get gating channels
          gatingChannels <- cciaObj()$flowGatingSet()$popChannels(x)
          
          cciaObj()$setPopAttr(
            popType(), popID, list(
              gateID = plotGateID,
              parent = cciaObj()$flowGatingSet()$popParent(x, "root"),
              valueName = popValueName(),
              path = x,
              xChannel = gatingChannels[[1]],
              yChannel = gatingChannels[[2]]
            )
          )
        }
        
        # save population map
        cciaObj()$savePopMap(popType())
      }
      
      # convert shape to gate coords
      flowShapeToGateCoords <- function(shapeObj) {
        gateCoords <- NULL
        
        if ("path" %in% names(shapeObj) && !is.na(shapeObj$path)) {
          # create freeform
          pathCoords <- stringr::str_split(shapeObj$path, "L")[[1]]
          
          # What does 'M' and 'Z' mean?
          pathCoords <- stringr::str_replace(pathCoords, "M", "")
          pathCoords <- stringr::str_replace(pathCoords, "Z", "")
          
          # convert to numbers
          pathCoords <- stringr::str_split(pathCoords, ",")
          pathCoords <- lapply(pathCoords, as.numeric)
          
          # are there at least 3 coordinates?
          gateCoords <- if (length(pathCoords) >= 3)
            list("path" = pathCoords)
          else
            NULL
        } else if ("x0" %in% names(shapeObj) && !is.na(shapeObj$x0)) {
          # create rectangle
          gateCoords <- list("rect" = list(
            xmin = shapeObj$x0,
            xmax = shapeObj$x1,
            ymin = shapeObj$y0,
            ymax = shapeObj$y1
          ))
        }
        
        gateCoords
      }
      
      # add gate for population
      flowAddGateWithShape <- function(id, shapeObj) {
        # get gate coords
        gateCoords <- flowShapeToGateCoords(shapeObj)
        
        if (!is.null(gateCoords)) {
          # get plot number
          boxIDs <- moduleManagers()$flowPlotManager$flowGatingBoxIDs(id, convertID = TRUE)
          
          # get flow plot
          flowPlot <- moduleManagers()$flowPlotManager$flowGatingPlots()[[boxIDs$i]]
          
          # add population to image
          popID <- cciaObj()$addPop(popType())
          
          # add to gating set
          gateID <- cciaObj()$flowGatingSet()$addPop(
            popID, gateCoords,
            flowPlot()$getPlotXchannel(flowName = TRUE),
            flowPlot()$getPlotYchannel(flowName = TRUE),
            parentPop = flowPlot()$getPlotPopPath())
          
          # compute
          cciaObj()$flowGatingSet()$recompute()
          
          popPath <- .flowPopPath(popID, flowPlot()$getPlotPopPath())
          names(popPath) <- popID
          
          # set gate ID for new population
          cciaObj()$setPopAttr(
            popType(), popID, list(
              gateID = gateID,
              valueName = popValueName(),
              parent = flowPlot()$getPlotPopPath(unlist = TRUE),
              path = popPath,
              xChannel = flowPlot()$getPlotXchannel(flowName = TRUE),
              yChannel = flowPlot()$getPlotYchannel(flowName = TRUE)
            )
          )
          
          if (globalManagers$projectManager()$getProjectType() != "flow") {
            # save population
            moduleManagers()$flowPlotManager$flowSavePops(popPath, completeDT = FALSE)
            
            # save population map
            cciaObj()$savePopMap(popType())
          }
          
          # update traces where population is shown
          for (x in moduleManagers()$flowPlotManager$flowGatingPlots()) {
            xIDs <- x()$getBoxIDs()
            
            # update subpopulations
            if (.flowPopIsParent(flowPlot()$getPlotPopPath(), x()$getPlotPopPath(), checkEqual = TRUE)) {
              # get leaves
              updateSelectInput(
                session, xIDs$popLeaves,
                # choices = cciaObj()$flowGatingSet()$popLeaves(
                #   flowPlot()$getPlotPopPath()),
                choices = .reverseNamedList(
                  cciaObj()$popLeaves(popType(), x()$getPlotPopPath())),
                selected = x()$getPlotPopLeaves()
              )
            }
          }
        }
        
        # TODO is this useful?
        # gs_pop_add(gs, flist, name = "remaining", negated = TRUE)
      }
      
      # set gate for population
      flowSetGateWithShape <- function(id, shapeObj, gateID) {
        # get gate coords
        gateCoords <- flowShapeToGateCoords(shapeObj)
        
        if (!is.null(gateCoords)) {
          # get plot number
          boxIDs <- moduleManagers()$flowPlotManager$flowGatingBoxIDs(id)
          
          # get flow plot
          flowPlot <- moduleManagers()$flowPlotManager$flowGatingPlots()[[boxIDs$i]]
          
          # add population to image
          popID <- cciaObj()$popIDsByAttrList(
            popType(), list(
              parent = .flowNormRootPath(flowPlot()$getPlotPopPath(), defaultVal = "root"),
              gateID = gateID,
              xChannel = flowPlot()$getPlotXchannel(flowName = TRUE),
              yChannel = flowPlot()$getPlotYchannel(flowName = TRUE)
            )
          )
          
          # get path
          popPath <- unlist(cciaObj()$popAttr(
            popType(), "path", popID = popID),
            use.names = FALSE)
          
          # set gate
          cciaObj()$flowGatingSet()$setPop(
            popPath, gateCoords,
            flowPlot()$getPlotXchannel(flowName = TRUE),
            flowPlot()$getPlotYchannel(flowName = TRUE)
          )
          
          # compute
          cciaObj()$flowGatingSet()$recompute()
          
          if (globalManagers$projectManager()$getProjectType() != "flow") {
            # save population
            moduleManagers()$flowPlotManager$flowSavePops(popPath, completeDT = FALSE)
            
            # save population map
            cciaObj()$savePopMap(popType())
          }
          
          if (length(flowPlot()$getPlotPopLeaves()) > 0) {
            popLeaves <- flowPlot()$getPlotPopLeaves()
            
            # get pop leaves to update
            popLeavesToUpdate <- popLeaves[startsWith(popLeaves, popPath)]
            
            # update plot leaves
             moduleManagers()$flowPlotManager$flowUpdatePopLeaves(flowPlot, popLeavesToUpdate)
          }
          
          # update traces where population is shown
          curBox <- moduleManagers()$flowPlotManager$flowGatingPlots()[[boxIDs$i]]
          curSeq <- seq(length(moduleManagers()$flowPlotManager$flowGatingPlots()))
          curSeq <- curSeq[curSeq != boxIDs$i]
          
          for (i in curSeq) {
            x <- moduleManagers()$flowPlotManager$flowGatingPlots()[[i]]
            xIDs <- x()$getBoxIDs()
            
            popLeaves <- c(
              popPath, cciaObj()$flowGatingSet()$popLeaves(popPath)
            )
            
            # match parameters
            resetPlot <- all(
              x()$getPlotPopPath() %in% popLeaves
            )
            
            # reset plot
            if (resetPlot == TRUE) {
              moduleManagers()$flowPlotManager$flowGatingPlotChangeTrace(
                x, forceRedraw = TRUE, removeTraceIDs = c())
            } else if (popPath %in% x()$getPlotPopLeaves()) {
              # update traces if the population is shown as subpopulation
              moduleManagers()$flowPlotManager$flowUpdatePopLeaves(x, popPath)
            }
          }
        }
      }
      
      ### Reactive values
      # when automatically generating gates
      # set this to FALSE
      flowListenToGating <- reactiveVal(TRUE)
      
      flowGatingPlotsLastShapes <- reactiveVal(0)
      flowNumGateUpdates <- reactiveVal(0)
      
      # population management
      popType <- reactive("flow")
      popValueName <- reactive("default")
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      # listen to shape edits
      flowGatingPlotsShapeEdits <- eventReactive(c(
        moduleManagers()$flowPlotManager$flowGatingPlotsRelayout(),
        moduleManagers()$flowPlotManager$flowAfterplot()
      ), {
        req(moduleManagers()$flowPlotManager$flowAfterplot())
        
        retVal <- moduleManagers()$flowPlotManager$flowGatingPlotsRelayout()
        
        # filter only on shapes
        for (i in names(retVal)) {
          if (any(is.na(stringr::str_match(names(retVal[[i]]), "shapes\\[[0-9]+\\]")))) {
            retVal[[i]] <- NULL
          }
        }
        
        if (length(retVal) > 0) retVal else NULL
      })
      
      # update image automatically when populations are gated
      updateImage <- eventReactive(c(
        # cciaObj()$flowGatingSet(),
        cciaObj(),
        flowGatingPlotsGatesMissing()
      ), {
        req(moduleManagers()$populationManager$autoUpdateImage())
        req(cciaObj())
        req(moduleManagers()$flowPlotManager$flowAfterplot())
        req(length(flowGatingPlotsGatesMissing()) == 0)
        
        # update image
        runif(1)
      })
      
      # listen to shape changes
      flowGatingPlotsShapeChanges <- eventReactive(c(
        moduleManagers()$flowPlotManager$flowGatingPlotsRelayout(),
        moduleManagers()$flowPlotManager$flowGatingPlotsRendered(),
        moduleManagers()$flowPlotManager$flowAfterplot()
      ), {
        # only gate when the user is actually adding and moving shapes
        # when the user changes the axis, shapes are redrawn
        # these events would also be picked up by the listener
        req(moduleManagers()$flowPlotManager$flowAfterplot())
        
        retVal <- moduleManagers()$flowPlotManager$flowGatingPlotsRelayout()
        
        # filter only on shapes
        for (i in names(retVal)) {
          for (j in names(retVal[[i]])) {
            if (j != "shapes") {
              retVal[[i]][[j]] <- NULL
            }
          }
        }
        
        if (length(retVal) > 0) retVal else NULL
      })
      
      # listen to added gates 
      flowGatingPlotsAddedGates <- eventReactive(flowGatingPlotsShapeChanges(), {
        req(flowListenToGating())
        req(flowGatingPlotsShapeChanges())
        req(moduleManagers()$flowPlotManager$flowGatingPlotsRendered())
        
        gateChanges <- flowGatingPlotsShapeChanges()
        
        # previous shapes
        prevShapes <- as.list(moduleManagers()$uiManager$pot_lyRelayoutCounts(
          flowGatingPlotsLastShapes()))
        
        # current shapes
        curShapes <- as.list(moduleManagers()$uiManager$pot_lyRelayoutCounts(
          gateChanges))
        
        # add missing populations
        shapeNames <- unique(c(names(prevShapes), names(curShapes)))
        
        for (x in shapeNames) {
          if (!x %in% names(prevShapes)) {
            prevShapes[[x]] <- 0
          }
          
          if (!x %in% names(curShapes)) {
            curShapes[[x]] <- 0
          }
        }
        
        if (!is.null(prevShapes) && length(prevShapes) > 0) {
          # sort by name
          prevShapes <- unlist(prevShapes[order(names(prevShapes))])
          curShapes <- unlist(curShapes[order(names(curShapes))])
          
          # check what has changed
          changedPlot <- names(curShapes)[curShapes > prevShapes]
        } else {
          changedPlot <- names(curShapes)
        }
        
        # return added gates
        if (!purrr::is_empty(changedPlot)) {
          # adjust name and get number of shapes
          nShapes <- nrow(gateChanges[[changedPlot]]$shapes)
          
          gateChanges[names(gateChanges) != changedPlot] <- NULL
          # gateChanges[[changedPlot]]$shapes <- gateChanges[[changedPlot]]$shapes[nShapes,]
          gateChanges[[changedPlot]] <- gateChanges[[changedPlot]]$shapes[nShapes,]
        } else {
          gateChanges <- NULL
        }
        
        gateChanges
      })
      
      ## Generic
      
      # population data
      popData <- reactive({
        req(cciaObj())
        
        moduleManagers()$populationManager$createPopData()
      })
      
      # use density plots
      flowUseFlowColours <- reactive({
        input$useFlowColours
      })
      
      # marker opacity
      flowMarkerOpacity <- reactive({
        input$markerOpacity
      })
      
      # which populations should be shown on the plots?
      flowGatingPlotsLeaveGates <- eventReactive(c(
        flowListenToGating(),
        moduleManagers()$flowPlotManager$flowGatingPlots(),
        moduleManagers()$flowPlotManager$flowPopLeavesUpdated(),
        cciaObj()$flowGatingSet()
      ), {
        req(moduleManagers()$flowPlotManager$flowGatingPlots())
        req(cciaObj()$flowGatingSet())
        
        leaveGates <- list()
        
        # check how many gates should be shown on each plot
        for (x in moduleManagers()$flowPlotManager$flowGatingPlots()) {
          boxIDs <- x()$getBoxIDs()
          
          # get populations shown in image
          curLeaves <- cciaObj()$flowGatingSet()$popDirectLeaves(x()$getPlotPopPath())
          
          for (curLeaf in curLeaves) {
            # get gate for current image
            matchAxis <- cciaObj()$flowGatingSet()$popGatedOnChannels(
              curLeaf, x()$getPlotChannels(flowName = TRUE)
            )
            
            if (matchAxis == TRUE) {
              # create list
              if (!boxIDs$plot %in% names(leaveGates)) {
                leaveGates[[boxIDs$plot]] <- list()
              }
              
              # add gate
              leaveGates[[boxIDs$plot]][[curLeaf]] <- cciaObj()$flowGatingSet()$popGate(curLeaf)
            }
          }
        }
        
        leaveGates
      })
      
      # check for plots with no gates
      flowGatingPlotsGatesNone <- reactive({
        req(moduleManagers()$flowPlotManager$flowGatingPlotsRendered())
        req(moduleManagers()$flowPlotManager$flowGatingPlots())
        
        # check what is shown on the plots
        gateShapes <- flowGatingPlotsShapeChanges()
        
        # list of gates
        leaveGates <- flowGatingPlotsLeaveGates()
        
        # check which plots need clearing
        numLeaveGates <- unlist(lapply(leaveGates, length))
        numGateShapes <- moduleManagers()$uiManager$pot_lyRelayoutCounts(gateShapes)
        
        unlist(lapply(
          names(numGateShapes),
          function(x) if (!x %in% names(numLeaveGates)) x
        ))
      })
      
      # check that gates finished rendering
      flowGatingPlotsGatesMissing <- reactive({
        req(moduleManagers()$flowPlotManager$flowGatingPlotsRendered())
        req(moduleManagers()$flowPlotManager$flowGatingPlots())
        
        # check what is shown on the plots
        gateShapes <- flowGatingPlotsShapeChanges()
        
        # isolate here - I could not get eventReactive to work consistently
        # flowGatingPlotsLeaveGates() cannot be called in eventReactive
        isolate({
          # ignore gate updates
          if (flowNumGateUpdates() > 0) {
            flowNumGateUpdates(flowNumGateUpdates() - 1)
          }
          
          req(flowNumGateUpdates() == 0)
        })
        
        # use the last changes if this is null
        # that will cause the previous plots to remember
        # their shapes when changing the number of plots
        if (is.null(gateShapes)) {
          gateShapes <- flowGatingPlotsLastShapes()
        }
        
        # list of gates
        leaveGates <- flowGatingPlotsLeaveGates()
        
        # check which gates need rendering
        numLeaveGates <- unlist(lapply(leaveGates, length))
        numGateShapes <- moduleManagers()$uiManager$pot_lyRelayoutCounts(gateShapes)
        
        gatesMissing <- list()
        for (i in names(numLeaveGates)) {
          x <- leaveGates[[i]]
          
          if (i %in% names(numGateShapes)) {
            if (numGateShapes[[i]] != numLeaveGates[[i]]) {
              gatesMissing[[i]] <- x
            }
          } else {
            gatesMissing[[i]] <- x
          }
        }
        
        gatesMissing
      })
      
      # remove gates from empty plots
      observeEvent(flowGatingPlotsGatesNone(), {
        # remove gates
        for (x in flowGatingPlotsGatesNone()) {
          moduleManagers()$uiManager$plot_lyClearShapesWithAnnotations(x)
        }
      })
      
      # add missing gates
      observeEvent(flowGatingPlotsGatesMissing(), {
        if (length(flowGatingPlotsGatesMissing()) == 0) {
          # set last changes
          flowGatingPlotsLastShapes(flowGatingPlotsShapeChanges())
          
          # enable listening
          flowListenToGating(TRUE)
        } else {
          # disable listening
          flowListenToGating(FALSE)
          
          newGates <- list()
          newPops <- list()
          
          gateProps <- list(
            editable = TRUE,
            xref = "x",
            yref = "y",
            layer = "above",
            opacity = "1",
            line = list(
              color = "#fafafa",
              width = 2,
              dash = "solid"
            ),
            fillcolor = "rgba(0, 0, 0, 0)",
            fillrule = "evenodd"
          )
          
          textProps <- list(
            xref = "x",
            yref = "y",
            layer = "above",
            opacity = "0.7",
            showarrow = FALSE,
            font = list(
              size = 15
            )
          )
          
          # go through missing gates
          for (i in names(flowGatingPlotsGatesMissing())) {
            # get gating plot
            boxIDs <- moduleManagers()$flowPlotManager$flowGatingBoxIDs(i, convertID = TRUE)
            gatingBox <- moduleManagers()$flowPlotManager$flowGatingPlots()[[boxIDs$i]]
            
            # create list
            newGates[[boxIDs$plot]] <- list()
            # newPops[[boxIDs$plot]] <- list()
            
            # prepare gates
            counter <- 1
            for (x in flowGatingPlotsGatesMissing()[[i]]) {
              if (class(x) == "polygonGate") {
                # make dataframe
                tmpDF <- as.data.frame(x@boundaries)
                
                # paste values together
                pathCoords <- paste(
                  tmpDF[,gatingBox()$getPlotXchannel(flowName = TRUE)],
                  tmpDF[,gatingBox()$getPlotYchannel(flowName = TRUE)],
                  sep = ",")
                
                # add start and end and collapse
                pathCoords[[1]] <- paste0("M", pathCoords[[1]])
                pathCoords[[length(pathCoords)]] <- paste0(pathCoords[[length(pathCoords)]], "Z")
                pathCoords <- paste(pathCoords, collapse = "L")
                
                # add gate to list
                newGates[[boxIDs$plot]][[counter]] <- append(
                  gateProps,
                  list(
                    type = "path",
                    path = pathCoords
                  )
                )
                
                # add population to list
                newPops[[boxIDs$plot]][[counter]] <- append(
                  textProps,
                  list(
                    text = x@filterId,
                    x = mean(tmpDF[,gatingBox()$getPlotXchannel(flowName = TRUE)]),
                    y = mean(tmpDF[,gatingBox()$getPlotYchannel(flowName = TRUE)])
                  )
                )
              } else if (class(x) == "rectangleGate") {
                # get min and max
                xmin <- x@min[[gatingBox()$getPlotXchannel(flowName = TRUE)]]
                ymin <- x@min[[gatingBox()$getPlotYchannel(flowName = TRUE)]]
                xmax <- x@max[[gatingBox()$getPlotXchannel(flowName = TRUE)]]
                ymax <- x@max[[gatingBox()$getPlotYchannel(flowName = TRUE)]]
                
                newRg <- c(xmin, xmax, ymin, ymax)
                
                # add gate to list
                newGates[[boxIDs$plot]][[counter]] <- append(
                  gateProps,
                  list(
                    type = "rect",
                    x0 = newRg[1],
                    x1 = newRg[2],
                    y0 = newRg[3],
                    y1 = newRg[4]
                  )
                )
                
                # add population to list
                newPops[[boxIDs$plot]][[counter]] <- append(
                  textProps,
                  list(
                    text = x@filterId,
                    x = newRg[1] + (newRg[2] - newRg[1]) / 2,
                    y = newRg[3] + (newRg[4] - newRg[3]) / 2
                  )
                )
              }
              
              counter <- counter + 1
            }
          }
          
          # draw gates
          if (length(newGates) > 0) {
            # ignore gate updates in missing gates
            flowNumGateUpdates(length(newGates))
            
            for (i in names(newGates)) {
              x <- newGates[[i]]
              y <- newPops[[i]]
              
              # add gates
              moduleManagers()$uiManager$plot_lyAddShapesWithAnnotations(
                i, x, annotations = y)
            }
          }
        }
      })
      
      # channel names
      channelNames <- reactive({
        cciaObj()$imChannelNames()
      })
      
      # selected ccia object
      cciaObj <- reactive({
        req(moduleManagers()$imageViewerManager$shownImage())
        
        moduleManagers()$imageViewerManager$shownImage()
      })
      
      # number of plots
      numFlowPlots <- reactive({
        input$numFlowPlots
      }) %>% debounce(cciaConf()$fcs$gating$plots$poll)
      
      # generate dataframe from selected image list
      imageData <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        moduleManagers()$imageSetManager$selectedSet()$summary(
          c("Attr"), withSelf = FALSE,
          uIDs = moduleManagers()$imageSetManager$filteredUIDs())
      })
      
      ### Observers - RxAction
      ## Event specific
      
      # DEBUG plots are rendered
      observeEvent(input$debugPlotsRendered, {
        moduleManagers()$flowPlotManager$debugPlotsRendered()
      })
      
      # listen to data selection
      # this will also trigger when the same image is chosen again
      # selected - user selects
      # shown - the image shown
      observeEvent(moduleManagers()$imageViewerManager$imageSelected(), {
        req(cciaObj())
        
        # require gating set
        req(cciaObj()$flowGatingSet())
        
        # collapse selection box
        js$collapseBox(session$ns("imageTableBox"))
        
        # set init flag
        moduleManagers()$flowPlotManager$initGatingBoxPlots(TRUE)
        
        if (globalManagers$projectManager()$getProjectType() != "flow") {
          # init all populations from root
          moduleManagers()$flowPlotManager$flowSavePops(
            "root", purge = TRUE, completeDT = FALSE)
          
          # save pop map
          cciaObj()$savePopMap(popType())
        }
      })
      
      # listen to viewer output
      observeEvent(globalManagers$viewerManager()$viewerOutput(), {
        req(moduleManagers()$imageViewerManager$shownImage())
        
        viewerOutput <- globalManagers$viewerManager()$viewerOutput()
        outputProcessed <- FALSE
        
        # check whether there is something to do
        if ("gatePopulationsSelectPoints" %in% names(viewerOutput)) {
          if (length(viewerOutput$gatePopulationsSelectPoints)) {
            # set selected labels
            moduleManagers()$flowPlotManager$viewerSelectedLabels(
              viewerOutput$gatePopulationsSelectPoints)
            
            outputProcessed <- TRUE
          }
        }
        
        # tell the viewer that the command was processed
        if (outputProcessed == TRUE){
          globalManagers$viewerManager()$clearViewerOutput()
        }
      })
      
      # listen to relayout
      observeEvent(flowGatingPlotsShapeChanges(), {
        # reset last changes if plots are empty
        if (is.null(
          moduleManagers()$uiManager$pot_lyRelayoutCounts(moduleManagers()$flowPlotManager$flowGatingPlotsRelayout())
        )) {
          # otherwise this will be triggered when the number
          # of plots are changed - new plots will be empty at first
          if (flowListenToGating() == TRUE) {
            flowGatingPlotsLastShapes(NULL)
          }
        }
      })
      
      # listen to edits
      observeEvent(flowGatingPlotsShapeEdits(), {
        req(flowGatingPlotsShapeEdits())
        # otherwise the gate is snapping back on some systems
        # req(flowListenToGating())
        
        # record shape changes
        flowGatingPlotsLastShapes(flowGatingPlotsShapeChanges())
        
        # get ids
        boxIDs <- moduleManagers()$flowPlotManager$flowGatingBoxIDs(
          names(flowGatingPlotsShapeEdits()), convertID = TRUE)
        
        # get gate ID
        shapeObj <- flowGatingPlotsShapeEdits()[[1]]
        # shapes start counting at '0'
        gateID <- as.numeric(stringr::str_extract(names(shapeObj), "[0-9]+")[[1]]) + 1
        
        # prepare shape object
        names(shapeObj) <- stringr::str_replace(
          names(shapeObj), "shapes\\[[0-9]+\\]\\.", "")
        
        flowSetGateWithShape(boxIDs$i, shapeObj, gateID)
        
        # clear plots where the same population and axis are shown
        # this will trigger redrawing of the gates
        curBox <- moduleManagers()$flowPlotManager$flowGatingPlots()[[boxIDs$i]]
        curSeq <- seq(length(moduleManagers()$flowPlotManager$flowGatingPlots()))
        curSeq <- curSeq[curSeq != boxIDs$i]
        
        for (i in curSeq) {
          x <- moduleManagers()$flowPlotManager$flowGatingPlots()[[i]]
          
          # match parameters
          resetPlot <- all(
            x()$getPlotXchannel() == curBox()$getPlotXchannel(),
            x()$getPlotYchannel() == curBox()$getPlotYchannel(),
            x()$getPlotPopPath() == curBox()$getPlotPopPath()
          )
          
          # reset plot
          if (resetPlot == TRUE) {
            moduleManagers()$uiManager$plot_lyClearShapesWithAnnotations(
              moduleManagers()$flowPlotManager$flowGatingBoxIDs(i)$plot
            )
          }
        }
      })
      
      # listen to added gates
      observeEvent(flowGatingPlotsAddedGates(), {
        # record shape changes
        flowGatingPlotsLastShapes(flowGatingPlotsShapeChanges())
        
        # go through the changed gates
        for (i in names(flowGatingPlotsAddedGates())) {
          # set gate for population
          flowAddGateWithShape(i, flowGatingPlotsAddedGates()[[i]])
        }
      })
      
      ## React to changes from population manager
      # deleted population
      observeEvent(moduleManagers()$populationManager$deletedPops(), {
        req(moduleManagers()$populationManager$deletedPops())
        
        popInfo <- moduleManagers()$populationManager$deletedPops()
        
        # delete population from gating set
        for (i in names(popInfo)) {
          x <- popInfo[[i]]
          
          # unlink population files
          moduleManagers()$flowPlotManager$flowUnlinkPops(x$path, popInfo)
          
          # remove from gating set
          cciaObj()$flowGatingSet()$delPop(x$path)
          
          # remove populations from image that had this population as parent
          popIDs <- cciaObj()$popIDsByAttr(
            popType(), "path", x$path, compareFun = "startsWith")
          for (y in popIDs) cciaObj()$delPop(popType(), y)
          
          # get parent population
          parentPop <- cciaObj()$flowGatingSet()$popParent(x$path, normaliseRoot = TRUE)
          
          # reset population information
          resetObjInfoFromGatingSet(parentPop)
          
          # remove from plot if selected as subpopulation
          # was the population selected in any set? 
          for (y in moduleManagers()$flowPlotManager$flowGatingPlots()) {
            boxIDs <- y()$getBoxIDs()
            
            # update leaves?
            popLeaves <- y()$getPlotPopLeaves()
            
            # remove population
            popLeaves <- popLeaves[popLeaves != x$path]
            
            # is the population selected as subpopulation?
            if (x$path %in% y()$getPlotPopLeaves()) {
              # get trace ID for population
              traceID <- c(which(names(y()$getPlotPopLeaves()) == i))
              
              # set leaves
              y()$setPlotPopLeaves(popLeaves, invalidate = FALSE)
              
              # update traces if no leaves selected
              if (purrr::is_empty(popLeaves)) {
                moduleManagers()$flowPlotManager$flowGatingPlotChangeTrace(
                  y, removeTraceIDs = c(1, 0))
              } else {
                # update trace if subpopulations are shown
                moduleManagers()$flowPlotManager$flowGatingPlotChangeTrace(
                  y, redrawIfLeaves = TRUE, leavesOnly = TRUE,
                  removeTraceIDs = traceID, ignoreNullPops = TRUE)
              }
              
              # save in image
              moduleManagers()$flowPlotManager$flowSavePopGatePlot(y, invalidate = FALSE)
            }
            
            # update select
            if (.flowPopIsParent(x$path, y()$getPlotPopPath())) { 
              updateSelectInput(
                session, boxIDs$popLeaves,
                # choices = cciaObj()$flowGatingSet()$popLeaves(x$parent),
                choices = .reverseNamedList(cciaObj()$popLeaves(popType(), y()$getPlotPopPath())),
                selected = popLeaves
              )
            }
          }
        }
      })
      
      # renamed population
      observeEvent(moduleManagers()$populationManager$renamedPops(), {
        req(moduleManagers()$populationManager$renamedPops())
        
        popInfo <- moduleManagers()$populationManager$renamedPops()
        
        # rename population in gating set
        for (i in names(popInfo)) {
          x <- popInfo[[i]]
          
          newPath <- cciaObj()$flowGatingSet()$renamePop(x$path, x$name)
          
          # revert if rename was not successful
          # ie/ there is already a population with that name
          if (is.null(newPath)) {
            cciaObj()$editPopName(popType(), i, x$prevName)
          } else {
            # reset information for changed populations
            resetObjInfoFromGatingSet(
              newPath, includeParent = TRUE, parentPath = x$path)
            
            savePopGatePlot <- FALSE
            
            # was the population selected in any set? 
            for (y in moduleManagers()$flowPlotManager$flowGatingPlots()) {
              boxIDs <- y()$getBoxIDs()
              selectedPopLeaves <- y()$getPlotPopLeaves()
              
              # is the population selected as subpopulation?
              shownLeaves <- if (!is.null(selectedPopLeaves) && length(selectedPopLeaves) > 0)
                startsWith(unlist(selectedPopLeaves, use.names = FALSE),
                           x$path)
              else
                FALSE
              
              if (any(shownLeaves)) {
                popLeaves <- y()$getPlotPopLeaves()
                
                # rename populations
                popLeaves[shownLeaves] <- unlist(lapply(
                  popLeaves[shownLeaves],
                  function(y) cciaObj()$flowGatingSet()$changeParentName(y, newPath)
                ))
                
                # update leaves
                y()$setPlotPopLeaves(
                  popLeaves, invalidate = FALSE)
                
                # update select
                selectedPopLeaves <- popLeaves
                
                # save in image
                savePopGatePlot <- TRUE
              }
              
              # is the population shown on any image?
              # then redraw the gates and annotations
              if (.flowNormRootPath(y()$getPlotPopPath(), "root") == x$parent) {
                moduleManagers()$uiManager$plot_lyClearShapesWithAnnotations(
                  boxIDs$plot
                )
              }
              
              # replace pop path
              newPlotPath <- .flowPopReplaceParent(
                y()$getPlotPopPath(), x$path, newPath, checkEqual = TRUE)
              
              # set new path in selection
              if (y()$getPlotPopPath() != newPlotPath) {
                # set pop ID as attribute
                names(newPlotPath) <- names(y()$getPlotPopPath())
                
                # update path
                y()$setPlotPopPath(newPlotPath, invalidate = FALSE)
                
                updateSelectInput(
                  session, boxIDs$popPath, selected = names(newPlotPath))
                
                # save in image
                savePopGatePlot <- TRUE
              }
              
              # save in image
              if (savePopGatePlot == TRUE) {
                moduleManagers()$flowPlotManager$flowSavePopGatePlot(y, invalidate = FALSE)
              }
              
              # update leaves if population is a child
              if (.flowPopIsParent(newPath, y()$getPlotPopPath(), checkEqual = TRUE)) { 
                updateSelectInput(
                  session, boxIDs$popLeaves,
                  # choices = cciaObj()$flowGatingSet()$popLeaves(x$parent),
                  choices = .reverseNamedList(cciaObj()$popLeaves(popType(), y()$getPlotPopPath())),
                  selected = selectedPopLeaves
                )
              }
            }
          }
        }
      })
      
      ## Generic
      
      ### UI Outputs
      ## Tables
      
      # images
      output$imageTable <- DT::renderDataTable({
        req(imageData())
        req(nrow(imageData()) > 0)
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          moduleManagers()$selectionManager$createSelectionColumn(),
          moduleManagers()$imageViewerManager$createShowImageColumn(),
          imageData()
          # moduleManagers()$taskManager$createTaskDataTableColumns() 
        ))
      })
      
      ## Plots
      
      ## Buttons
      
      ## Other
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet",
        "imageViewer", "population", "shapes", "flowPlot")
      managerConf = list(
        moduleName = id,
        imageData = imageData,
        cciaObj = cciaObj,
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        imageViewer = list(
          napariModule = "gate_populations"
        ),
        task = list(
          funLabel = "Gating method",
          runTaskCombinedSelect = TRUE
        ),
        population = list(
          popData = popData,
          popType = popType,
          updateImage = updateImage,
          enableEditPopName = TRUE,
          enableFilterPopulation = TRUE
        ),
        shapes = list(
        ),
        flowPlot = list(
          popType = popType,
          numFlowPlots = numFlowPlots,
          flowUseFlowColours = flowUseFlowColours,
          flowMarkerOpacity = flowMarkerOpacity
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
