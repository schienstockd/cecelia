#' @description Shiny UI must be a function for bookmarking
#' @param request bookmark request
#' @examples
#' TODO
ui <- function(request) {
  # --- sidebar ---
  sidebar <- dashboardSidebar(
    sidebarMenu(id = "sidebar",
      
      htmlOutput("projectName") %>%
        tagAppendAttributes(class = 'txt-medium-centred'),
      menuItemOutput("menuCreateProject"),
      menuItemOutput("menuStaticAnalysis"),
      menuItemOutput("menuLiveAnalysis"),
      menuItemOutput("menuFlowAnalysis"),
      menuItemOutput("menuPlotCanvas"),
      menuItemOutput("menuSettings"),
      
      # --- Viewer settings ---
      tags$hr(),
      box(
        # id = ns("viewerParamsBox"),
        title = "Viewer",
        solidHeader = TRUE,
        collapsible = TRUE, 
        collapsed = TRUE, 
        status = "primary",
        width = 12,
        fluidRow(
          fluidRow(
            column(4, actionButton("viewerSaveLayerProps", "Save")),
            column(4, actionButton("viewerAddAnimationPane", "Recorder"))
          ),
          checkboxGroupInput(
            "viewerParams", NULL,
            c(
              # "Split channels" = "useChannelAxis",
              # "Viewer" = "showViewer",
              "Reload Image" = "reloadImage",
              "Original" = "showOriginal",
              "Layers" = "layersVisible",
              "3D" = "show3D",
              "Labels" = "showLabels",
              "Points" = "showPoints",
              "Tracks" = "showTracks",
              "Populations" = "showPops",
              "Neighbours" = "showNeighbours",
              "Branching" = "showBranching",
              "Shapes" = "showShapes",
              "Lazy" = "asDask",
              "Squeeze" = "squeeze",
              "Reset" = "resetViewer"
              # "Downsample Z" = "downsampleZ"
              ),
            selected = c(
              "layersVisible", "showViewer", "showLabels", "showTracks",
              "showPoints", "showPops", "showNeighbours", "showShapes", "asDask",
              "showBranching", "resetViewer"
              )
            ),
          sliderInput("viewerMultiscales", "Pyramids", min = 1, max = 12, value = 12),
          selectInput(
            "viewerBranchingProperty", "Branching property",
            list(
              "Type" = "type",
              "Weight" = "weight"
            ), selected = "type")
        )
      ),
      
      # --- Load/Save project ---
      box(
        # id = ns("viewerParamsBox"),
        title = "Project",
        solidHeader = TRUE,
        collapsible = TRUE, 
        collapsed = TRUE, 
        status = "primary",
        width = 12,
        fluidRow(
          column(6,
                 actionButton("loadProject", "Load"),
                 actionButton("importProject", "Import")
          ),
          column(6,
                 disabled(actionButton("saveProject", "Save")),
                 disabled(actionButton("exportProject", "Export"))
          )
        )
      ),
      box(
        # id = ns("viewerParamsBox"),
        title = "Version",
        solidHeader = TRUE,
        collapsible = TRUE, 
        collapsed = TRUE, 
        status = "primary",
        width = 12,
        fluidRow(
          column(6,
                 disabled(actionButton("loadVersion", "Load"))
          ),
          column(6,
                 disabled(actionButton("createVersion", "Create"))
          )
        )
      ),
      fluidRow(
        column(6,
               actionButton("shutdown", "Shutdown",
                            class = btnCLASS_IMPORTANT)
        )
      )
    )
  )
  
  # --- main body ---
  body <- dashboardBody(
    # tags$img(
    #   src = file.path(cciaConf()$wwwDirs$pathToImg, "ninja_stack.png"),
    #   style = 'position:absolute'
    # ),
    useShinyjs(),
    extendShinyjs(
      file.path("JS", "shinyjsExtended.js"),
      functions = c("collapseBox")),
    
    # toggle treeview of dashboard
    # https://stackoverflow.com/questions/32465177/how-to-manually-expand-a-submenu-in-a-shiny-dashboard-side-bar
    
    # change theme
    # shinyDashboardThemes(
    #   theme = "grey_dark"
    # ),
    
    tabItems(
      # create project
      tabItem(
        tabName = "createProject",
        .createProjectUI("init")
      ),
      
      # general analysis
      tabItem(
        tabName = "importImages",
        .importImagesUI("importImages")
      ),
      tabItem(
        tabName = "importFlow",
        .importFlowUI("importFlow")
      ),
      tabItem(
        tabName = "manageMetadata",
        .manageMetadataUI("metadata")
      ),
      tabItem(
        tabName = "segmentImages",
        .segmentImagesUI("segment")
      ),
      
      # specific to static images?
      tabItem(
        tabName = "gatePopulations",
        .gatePopulationsUI("gatePopulations")
      ),
      
      tabItem(
        tabName = "clustPopulations",
        .clustPopulationsUI("clustPopulations")
      ),
      
      tabItem(
        tabName = "clustRegions",
        .clustRegionsUI("clustRegions")
      ),
      
      tabItem(
        tabName = "spatialAnalysis",
        .spatialAnalysisUI("spatialAnalysis")
      ),
      
      tabItem(
        tabName = "pixelClassification",
        .pixelClassificationUI("pixcl")
      ),
      
      tabItem(
        tabName = "signalAnalysis",
        .signalAnalysisUI("signalAnalysis")
      ),
      
      # specific to live images?
      tabItem(
        tabName = "cleanupImages",
        .cleanupImagesUI("cleanupImages")
      ),
      tabItem(
        tabName = "trainModels",
        .trainModelsUI("trainModels")
      ),
      tabItem(
        tabName = "trackingImages",
        .trackingImagesUI("tracking")
      ),
      tabItem(
        tabName = "behaviourAnalysis",
        .behaviourAnalysisUI("behaviourAnalysis")
      ),
      
      # canvas
      tabItem(
        tabName = "plotCharts",
        .plotChartsUI("plotCharts")
      ),
      tabItem(
        tabName = "plotInteractionHeatmaps",
        .plotChartsUI("plotInteractionHeatmaps")
      ),
      tabItem(
        tabName = "plotHeatmaps",
        .plotChartsUI("plotHeatmaps")
      ),
      tabItem(
        tabName = "plotClustersUMAP",
        .plotChartsUI("plotClustersUMAP")
      ),
      tabItem(
        tabName = "plotFlowGating",
        .plotChartsUI("plotFlowGating")
      ),
      tabItem(
        tabName = "plotPopDensities",
        .plotChartsUI("plotPopDensities")
      ),
      # tabItem(
      #   tabName = "plotTrackClustersUMAP",
      #   .plotChartsUI("plotTrackClustersUMAP")
      # ),
      
      # settings
      tabItem(
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
      title = shinyDashboardLogo(
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
