#' @description Shiny UI must be a function for bookmarking
#' @param request bookmark request
#' @examples
#' TODO
ui <- function(request) {
  # --- sidebar ---
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id = "sidebar",
      
      htmlOutput("projectName") %>%
        tagAppendAttributes(class = 'txt-medium-centred'),
      shinydashboard::menuItemOutput("menuCreateProject"),
      shinydashboard::menuItemOutput("menuStaticAnalysis"),
      shinydashboard::menuItemOutput("menuLiveAnalysis"),
      shinydashboard::menuItemOutput("menuFlowAnalysis"),
      shinydashboard::menuItemOutput("menuSettings"),
      
      # --- Viewer settings ---
      tags$hr(),
      fluidRow(
        column(
          6,
          checkboxGroupInput(
            "viewerParams", "Viewer",
            c(
              # "Split channels" = "useChannelAxis",
              # "Viewer" = "showViewer",
              "Original" = "showOriginal",
              "Layers" = "layersVisible",
              "3D" = "show3D",
              "Labels" = "showLabels",
              "Points" = "showPoints",
              "Tracks" = "showTracks",
              "Populations" = "showPops",
              "Neighbours" = "showNeighbours",
              "Shapes" = "showShapes",
              "Lazy" = "asDask"
              ),
            selected = c(
              "layersVisible", "showViewer", "showLabels", "showTracks",
              "showPoints", "showPops", "showNeighbours", "showShapes", "asDask"
              )
            )
        ),
        column(
          6,
          selectInput("viewerMultiscales", "Pyramids",
                      seq(1, 4), selected = 4),
          actionButton("viewerAddAnimationPane",
                       "Recorder"),
          tags$br(),
          actionButton("closeViewer", "Close",
                       class = btnCLASS_IMPORTANT)
        )
      ),
      tags$hr(),
      
      # --- Load/Save project ---
      fluidRow(
        column(3,
               tags$div("Project", class = 'txt-default-centred')
               ),
        column(3,
               actionButton("loadProject", "Load"),
               actionButton("importProject", "Import")
               ),
        column(3,
               shinyjs::disabled(actionButton("saveProject", "Save")),
               shinyjs::disabled(actionButton("exportProject", "Export")),
               )
      ),
      fluidRow(
        column(3,
               tags$div("Version", class = 'txt-default-centred')
               ),
        column(3,
               shinyjs::disabled(actionButton("loadVersion", "Load"))
               ),
        column(3,
               shinyjs::disabled(actionButton("createVersion", "Create"))
               )
      ),
      tags$hr(),
      fluidRow(
        column(6,
               actionButton("shutdown", "Shutdown",
                            class = btnCLASS_IMPORTANT)
        )
      )
    )
  )
  
  
  # --- main body ---
  body <- shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      file.path("JS", "shinyjsExtended.js"),
      functions = c("collapseBox")),
    
    # toggle treeview of dashboard
    # https://stackoverflow.com/questions/32465177/how-to-manually-expand-a-submenu-in-a-shiny-dashboard-side-bar
    
    # change theme
    dashboardthemes::shinyDashboardThemes(
      theme = "grey_dark"
    ),
    
    shinydashboard::tabItems(
      # create project
      shinydashboard::tabItem(
        tabName = "createProject",
        .createProjectUI("init")
      ),
      
      # general analysis
      shinydashboard::tabItem(
        tabName = "importImages",
        .importImagesUI("import")
      ),
      shinydashboard::tabItem(
        tabName = "importFlow",
        .importFlowUI("importFlow")
      ),
      shinydashboard::tabItem(
        tabName = "manageMetadata",
        .manageMetadataUI("metadata")
      ),
      shinydashboard::tabItem(
        tabName = "segmentImages",
        .segmentImagesUI("segment")
      ),
      
      # specific to static images?
      shinydashboard::tabItem(
        tabName = "gatePopulations",
        .gatePopulationsUI("gatePopulations")
      ),
      
      shinydashboard::tabItem(
        tabName = "clustPopulations",
        .clustPopulationsUI("clustPopulations")
      ),
      
      shinydashboard::tabItem(
        tabName = "spatialAnalysis",
        .spatialAnalysisUI("spatialAnalysis")
      ),
      
      shinydashboard::tabItem(
        tabName = "pixelClassification",
        .pixelClassificationUI("pixcl")
      ),
      
      shinydashboard::tabItem(
        tabName = "signalAnalysis",
        .signalAnalysisUI("signalAnalysis")
      ),
      
      # specific to live images?
      shinydashboard::tabItem(
        tabName = "cleanupImages",
        .cleanupImagesUI("cleanupImages")
      ),
      shinydashboard::tabItem(
        tabName = "trainModels",
        .trainModelsUI("trainModels")
      ),
      shinydashboard::tabItem(
        tabName = "trackingImages",
        .trackingImagesUI("tracking")
      ),
      shinydashboard::tabItem(
        tabName = "behaviourAnalysis",
        .behaviourAnalysisUI("behaviourAnalysis")
      ),
      
      # settings
      shinydashboard::tabItem(
        tabName = "projectSettings",
        .projectSettingsUI("global")
      )
    ),
    
    # --- JS ---
    tags$head(includeScript(
      file.path(cciaConf()$wwwDirs$pathToJS, "app.js"))),
    
    # --- CSS ---
    tags$head(
      tags$link(
        rel = "stylesheet", type = "text/css",
        href = file.path(cciaConf()$wwwDirs$pathToCss, "dark_mode.css"))
    )
  )
  
  # --- whole page ---
  dashboardPage(
    title = "Cecelia",
    dashboardHeader(
      # change logo
      title = shinydashboard::shinyDashboardLogo(
        theme = "grey_dark",
        boldText = "Cecelia",
        mainText = "App",
        badgeText = CCIAVERSION
      )
    ),
    sidebar,
    body
  )
}
