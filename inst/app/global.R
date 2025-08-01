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
# DEBUG_SHOW_VIEWER <- FALSE
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
  "global-mfluxTokenFileChoose",
  "importImages-imagesToImport",
  "importImages-imagesToImport-modal",
  "importImages-foldersToImport",
  # add settings to the values manually
  "settingsManager",
  # exclude passwords
  "global-labServerSmbPwd", "global-projectHPCpwd",
  "global-runHPCTask",
  "global-runMfluxTask",
    # exclude buttons
  "loadProject", "saveProject",
  "exportProject", "importProject",
  "loadVersion", "createVersion",
  "deleteSelectedProjectConfirmed",
  "shutdown", "restart", "closeViewer", "viewerAddAnimationPane",
  "viewerSaveLayerProps", 
  "importImages-deleteSetConfirmed",
  "importImages-deleteUIDConfirmed",
  "importImages-taskRunAll",
  "importImages-taskHPCsync", "segment-taskHPCsync",
  "gatePopulations-taskHPCsync", "clustPopulations-taskHPCsync",
  "importImages-createSet", "importImages-refreshSet", "importImages-reloadSet",
  "importImages-resetMetadata",
  "global-setupLabServerTest", "global-udateHPCUserLibs",
  "metadata-createAttribute", "metadata-showImage", "metadata-reloadSet",
  "metadata-assignTimeIntervals",
  "segment-showImage", "segment-segmentVp", "segment-reloadSet",
  "clustPopulations-createPop", "clustPopulations-reloadSet",
  # tracking
  "trackingCorrection-pointsOpAdd",
  "trackingCorrection-pointsOpRm",
  "trackingCorrection-pointsOpSave",
  "trackingCorrection-tracksOpJoin",
  "trackingCorrection-edtTracksSave",
  "trackingCorrection-trackEditRollback",
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
