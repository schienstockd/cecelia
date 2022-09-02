#' @description Server to manage metadata
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.manageMetadataServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Classic variables
      
      ### Functions
      
      ### Reactive values
      # general
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      
      ## Generic
      
      setUIDs <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        
        # get selected objects
        uIDs <- moduleManagers()$selectionManager$selectedUIDs()
        
        if (length(uIDs) == 0){
          uIDs <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectUIDs()
        }
        
        uIDs
      })
      
      # generate dataframe from selected image list
      imageData <- reactive({
        req(moduleManagers()$imageSetManager$selectedSet())
        req(length(moduleManagers()$imageSetManager$selectedSet()) > 0)
        
        moduleManagers()$imageSetManager$selectedSet()$summary(
          c("Name", "ChannelNames", "Attr"), withSelf = FALSE,
          uIDs = moduleManagers()$imageSetManager$filteredUIDs())
      })
      
      # get attribute names
      cciaAttributeNames <- reactive({
        c(
          moduleManagers()$imageSetManager$selectedSet()$cciaObjectsAttrNames(),
          moduleManagers()$imageSetManager$selectedSet()$cciaObjectsChannelNames()
        )
      })
      
      # supply napari module
      napariModule <- reactive({
        NULL
      })
      
      ### Observers - RxAction
      ## Event specific
      
      # reset metadata from file
      observeEvent(input$resetMetadata, {
        req(moduleManagers()$imageSetManager$selectedSet())
        
        # update objects metadata
        cciaObjects <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectsByUIDs(setUIDs())
        
        progress <- Progress$new()
        progress$set(message = "Get Metadata ... ", value = 0)
        
        numObjects <- length(cciaObjects)
        counter <- 1
        
        for (curObj in cciaObjects) {
          progress$set(
            message = sprintf("Image %d/%d", counter, numObjects),
            value = (counter/numObjects) * 100
            )
          
          # refresh image to be sure everything is up to date
          curObj()$retrieveState()
          
          curObj()$resetMetaFromFile()
          
          counter <- counter + 1
        }
        
        progress$close()
      })
      
      # toggle buttons based on selected attribute
      observeEvent(input$selectAttribute, {
        # toggle UI
        if (input$selectAttribute != ""){
          enable("deleteAttribute")
          enable("singleValue")
          enable("regexpValue")
          enable("assignSingleValue")
          enable("assignRegexpValue")
          enable("assignGroupSequence")
        } else {
          disable("deleteAttribute")
          disable("singleValue")
          disable("regexpValue")
          disable("assignSingleValue")
          disable("assignRegexpValue")
          disable("assignGroupSequence")
        }
      })
      
      # create attribute
      observeEvent(input$createAttribute, {
        req(input$attributeName)
        
        # add attribute to image set
        moduleManagers()$imageSetManager$selectedSet()$addAttrForCciaObjects(input$attributeName)
      })
      
      # delete attribute
      observeEvent(input$deleteAttribute, {
        req(input$selectAttribute)
        
        # delete attribute from image set
        moduleManagers()$imageSetManager$selectedSet()$delAttrForCciaObjects(input$selectAttribute)
      })
      
      # assign regexp to attribute
      observeEvent(input$assignRegexpValue, {
        req(input$selectAttribute)
        req(input$regexpValue)
        
        cciaObjects <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectsByUIDs(setUIDs())
        
        # apply regexp to name of image
        attrVals <- lapply(cciaObjects, function(x) {
          if (input$regexpSource == "oriFilepath")
            stringr::str_extract(x()$oriFilepath(), input$regexpValue)
          else
            stringr::str_extract(x()$getCciaName(), input$regexpValue)
        })
        
        # set attribute for selected images
        if (input$selectAttribute %in% moduleManagers()$imageSetManager$selectedSet()$cciaObjectsAttrNames()) {
          moduleManagers()$imageSetManager$selectedSet()$editAttrForCciaObjects(
            input$selectAttribute, attrVals, cciaObjects)
        }
      })
      
      # assign value to attribute
      observeEvent(input$assignSingleValue, {
        req(input$selectAttribute)
        req(input$singleValue)
        
        cciaObjects <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectsByUIDs(setUIDs())
        
        # prepare values
        attrVals <- rep(input$singleValue, length(setUIDs()))
        names(attrVals) <- setUIDs()
        
        # set attribute for selected images
        if (input$selectAttribute %in% moduleManagers()$imageSetManager$selectedSet()$cciaObjectsAttrNames()) {
          moduleManagers()$imageSetManager$selectedSet()$editAttrForCciaObjects(
            input$selectAttribute, attrVals, cciaObjects)
        }
        
        # set channel names for selected images
        if (input$selectAttribute %in% moduleManagers()$imageSetManager$selectedSet()$cciaObjectsChannelNames()) {
          moduleManagers()$imageSetManager$selectedSet()$editChannelNamesForCciaObjects(
            input$selectAttribute, attrVals, cciaObjects,
            checkLength = globalManagers$projectManager()$getProjectType() != "flow",
            updateFlowGatingSet = globalManagers$projectManager()$getProjectType() == "flow"
            )
        }
      })
      
      # assign channel names to images
      observeEvent(input$assignChannelNameList, {
        req(input$channelNameList)
        
        # prepare values
        channelNames <- unlist(strsplit(input$channelNameList, "\\n"))
        
        # set channel names for selected images
        for (x in moduleManagers()$imageSetManager$selectedSet()$cciaObjectsByUIDs(setUIDs())) {
          x()$setImChannelNames(
            channelNames,
            checkLength = globalManagers$projectManager()$getProjectType() != "flow",
            updateFlowGatingSet = globalManagers$projectManager()$getProjectType() == "flow"
            )
        }
      })
      
      # assign group sequences
      observeEvent(input$assignGroupSequences, {
        # get attributes for images
        attrNames <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectsAttrNames()
        # remove 'GroupSeq'
        attrNames <- attrNames[attrNames != "GroupSeq"]
        
        # group images by attributes
        groupSeq <- imageData() %>%
          group_by(across(all_of(attrNames))) %>%
          dplyr::mutate(GroupSeq = 1:n())
        
        groupVals <- groupSeq$GroupSeq
        names(groupVals) <- groupSeq$uID
        
        # Copy to objects
        moduleManagers()$imageSetManager$selectedSet()$delAttrForCciaObjects("GroupSeq")
        moduleManagers()$imageSetManager$selectedSet()$addAttrForCciaObjects("GroupSeq", groupVals)
      })
      
      # copy channel names to all
      observeEvent(input$copyChannelNamesToAll, {
        # get selected objects
        uIDs <- moduleManagers()$selectionManager$selectedUIDs()
        
        # only take the first one
        if (length(uIDs) > 0) {
          cciaObject <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectByUID(uIDs[[1]])[[1]]
          
          # get channel names
          channelNames <- cciaObject()$imChannelNames()
          
          # go through all objects and copy
          uIDs <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectUIDs()
          cciaObjects <- moduleManagers()$imageSetManager$selectedSet()$cciaObjectsByUIDs(uIDs)
          
          for (curName in names(channelNames)) {
            curVal <- channelNames[[curName]]
            
            # prepare values
            chnVals <- rep(curVal, length(uIDs))
            names(chnVals) <- uIDs
            
            moduleManagers()$imageSetManager$selectedSet()$editChannelNamesForCciaObjects(
              curName, chnVals, cciaObjects,
              checkLength = globalManagers$projectManager()$getProjectType() != "flow",
              updateFlowGatingSet = globalManagers$projectManager()$getProjectType() == "flow"
              )
          }
        }
      })
      
      ## Generic
      
      ### UI Outputs
      ## Tables
      output$imageTable <- DT::renderDataTable({
        req(imageData())
        req(nrow(imageData()) > 0)
        
        # get table
        moduleManagers()$uiManager$dataTable(list(
          moduleManagers()$selectionManager$createSelectionColumn(),
          moduleManagers()$imageViewerManager$createShowImageColumn(),
          imageData()
        ))
      })
      
      ## Plots
      
      ## Buttons
      
      ## Other
      
      # select an attribute
      output$selectAttribute <- renderUI({
        selectInput(
          session$ns("selectAttribute"), NULL,
          cciaAttributeNames(), input$selectAttribute)
      })
      
      ### Managers
      # init managers
      managerNames = c(
        "ui", "input", "selection", "task", "imageSet", "imageViewer")
      managerConf = list(
        moduleName = id,
        imageData = imageData,
        imageSet = list(
          enableAddition = TRUE,
          enableDeletion = TRUE
        ),
        input = list(
          sourceDirectory = file.path(cciaConf()$tasks$inputDefinitions, id)
        ),
        task = list(
          funLabel = "Import method"
        )
      )
      
      moduleManagers <- createModuleManager(
        input, output, session, globalManagers, id, managerNames, managerConf)
    }
  )
}
