options(shiny.fullstacktrace = TRUE)

# source all files from subdirectories
# TODO is there a better way of doing this?
appSources <- c(
  "constantsCore.R",
  # TODO should the libraries be added to 'depends' in DESCRIPTION
  # rather than here to load into global workspace?
  list.files("lib", pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files("helpers", pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files("utils", pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files("modules", pattern = ".R$", recursive = TRUE, full.names = TRUE)
)

for (x in appSources) {
  source(x)
}

# DEBUG
DEBUG_FILE <- file.path(cciaConf()$dirs$debug, cciaConf()$files$debug)
DEBUG_SHOW_VIEWER <- TRUE
DEBUG_NO_VIEWER_SHUTDOWN <- FALSE
DEBUG_SHOW_TASK_RESULT <- FALSE
DEBUG_SHOW_TASK_EXPRESSION <- TRUE

# shiny options ?
enableBookmarking("server")

# encryption
# CCIA_SESSION_KEY <- PKI.genRSAkey(cciaConf()$encryption$keySize)
CCIA_SESSION_KEY <- PKI::PKI.genRSAkey(2048)

# define analysis module pages
SHINY_ANALYSIS_MODULES <- c(
  "behaviourAnalysis", "cleanupImages",
  "clustPopulations", "gatePopulations",
  "import", "segment", "manageMetadata",
  "spatialAnalysis", "trackingImages",
  "trainModels"
)

# exclude for bookmarking
SHINY_BOOKMARK_EXCLUDE <- c(
  # this create the following error
  # Warning: Error in writeImpl: Text to be written must be a length-one character vector
  # That's ok - the path is set anyway by loading bookmark
  "init-projectPath", "init-projectPath-modal", "init-createProject",
  "global-projectHPCsshKeyfileChoose",
  "import-imagesToImport", "import-imagesToImport-modal",
  # add settings to the values manually
  "settingsManager",
  # exclude passwords
  "global-labServerSmbPwd", "global-projectHPCpwd",
  # exclude buttons
  "loadProject", "saveProject",
  "loadVersion", "createVersion",
  "shutdown", "restart", "closeViewer", "viewerAddAnimationPane",
  "import-taskRunAll",
  "import-taskHPCsync", "segment-taskHPCsync",
  "gatePopulations-taskHPCsync", "clustPopulations-taskHPCsync",
  "import-createSet", "import-refreshSet", "import-reloadSet",
  "import-resetMetadata",
  "global-setupLabServerTest", "global-udateHPCUserLibs",
  "metadata-createAttribute", "metadata-showImage", "metadata-reloadSet",
  "segment-showImage", "segment-segmentVp", "segment-reloadSet",
  "clustPopulations-createPop", "clustPopulations-reloadSet",
  # exclude log fields
  "segment-closeLog",
  # exclude fields from flow
  "gatePopulations-resetImageLabelSelection",
  "gatePopulations-debugPlotsRendered",
  paste0("gatePopulations-gatingBoxPlot_", seq(1, 10)),
  paste0("gatePopulations-gatingBoxAxisX_", seq(1, 10)),
  paste0("gatePopulations-gatingBoxAxisY_", seq(1, 10)),
  paste0("gatePopulations-gatingBoxAxisXScale_", seq(1, 10)),
  paste0("gatePopulations-gatingBoxAxisYScale_", seq(1, 10)),
  paste0("gatePopulations-gatingBoxPopSelect_", seq(1, 10)),
  # exclude version information forms
  "createVersionComment", "createVersionSubmit",
  "deleteSelectedVersion", "loadSelectedVersion",
  # exclude task manager
  # "runningTasks", "queuedTasks"
  # exclude toggle
  paste0(SHINY_ANALYSIS_MODULES, "-toggleRows"),
  paste0(SHINY_ANALYSIS_MODULES, "-propagatePopMapping")
)

# Hotkeys
selectionHotkeys <- c(
  "shift"
)
