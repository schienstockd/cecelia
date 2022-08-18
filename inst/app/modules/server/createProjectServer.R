#' @description Server to create new project
#' @import shinyFiles
#' 
#' @param id character of module ID
#' @param parent Session of parent context
#' @param globalManagers list of global managers
#' @examples
#' TODO
.createProjectServer <- function(id, parent, globalManagers) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ### Reactive values
      projectPath <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      projectName <- eventReactive(input$projectName, {
        # do check if required
        input$projectName
      })
      
      projectType <- eventReactive(input$projectType, {
        # do check if required
        input$projectType
      })
      
      ## Generic
      
      ### Observers - RxAction
      ## Event specific
      
      # enable submit button if all inputs are met
      observeEvent(projectPath(), {
        req(projectPath())
        
        # enable button
        enable("createProject")
      })
      
      # # listen to path selection
      # observeEvent(input$projectPath, {
      #   req(input$projectPath)
      #   # check that a directory was selected
      #   req("root" %in% names(input$projectPath))
      #   
      #   curPath <- joinSelectedPath(
      #     input$projectPath, useConfigVolumes = TRUE)
      #   
      #   # check that the directory is empty
      #   if(length(list.files(
      #     curPath, include.dirs = TRUE)) > 0){
      #     showNotification(
      #       msgSELECT_EMPTY_DIR[1],
      #       type = msgSELECT_EMPTY_DIR[2])
      #   } else {
      #     projectPath(curPath)
      #   }
      # })
      
      # create the first bookmark when creating a project
      observeEvent(input$createProject, {
        req(projectPath)
        
        # is there a project with the same name?
        if (!globalManagers$projectManager()$isProjectNameUnique(projectName())) {
          showNotification(
            msgPROJECT_UNIQUE_NAME[1],
            type = msgPROJECT_UNIQUE_NAME[2])
        } else {
          # get uID
          projectUID <- genUID(6)
          
          # create uID
          globalManagers$projectManager()$setProjectUID(projectUID)
          
          # save settings
          globalManagers$projectManager()$setProjectName(projectName())
          globalManagers$projectManager()$setProjectType(projectType())
          # globalManagers$projectManager()$setProjectPath(projectPath())
          
          # create project entry
          globalManagers$projectManager()$createProject()
          
          # create 'zero' task dir
          globalManagers$projectManager()$persistentObjectDirectory(version = 0)
          
          # save project
          session$doBookmark()
          
          # show import images tab
          # by default, this will be import images
          tabImportImages <- paste0(
            globalManagers$projectManager()$getProjectType(),
            "ImportImages")
          
          showTab(
            inputId = "sidebar",
            target = tabImportImages,
            select = TRUE,
            session = parent)
          
          showNotification(
            msgPROJECT_CREATED[1],
            type = msgPROJECT_CREATED[2])
        }
      })
      
      ## Generic
      
      ### UI Outputs
      ## Tables
      
      ## Plots
      
      ## Buttons
      
      ## Other
      shinyDirChoose(
        input, "projectPath",
        roots = cciaConf()$volumes,
        session = session)
    }
  )
}
