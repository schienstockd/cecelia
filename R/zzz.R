# load global config
# https://stackoverflow.com/a/12605694
# TODO not sure if there is a better way to do this
pkg.env <- new.env()

# Functions to run when package is loaded
.onLoad <- function(libname, pkgname) {
  # prepare paths
  pkg.env$cfg <- config::get(
    file = system.file("config.yml", package = "cecelia"))
}

#' @description Expose config
#' @export
cciaConf <- function() {
  pkg.env$cfg
}

#' @description Expose path
#' @export
cciaPath <- function() {
  pkg.env$path
}

#' @description Expose environment, eg/ for python classes
#' @export
cciaEnv <- function() {
  pkg.env
}

#' @description Setup cecelia working directory
#' @param path character to define working directory of cecelia
#' @export
cciaSetup <- function(path) {
  # set path in environment
  pkg.env$path <- path
  
  # create directory if not present
  dir.create(path, showWarnings = FALSE)
  
  # copy in custom.yml to be modified
  file.copy(
    system.file("custom.yml", package = "cecelia"),
    file.path(path)
  )
}

#' @description Create conda environment for cecelia
#' @param envName character for environment name
#' @param envType character for environment type. Any of c("image", "flow")
#' @export
cciaCondaCreate <- function(envName = "r-cecelia-env", envType = "image") {
  envFile <- system.file(
    file.path("py-env", "conda-env-image.yml"),
    package = "cecelia")
  pyModulesFile <- system.file(
    file.path("py-env", "init-py-modules-image.txt"),
    package = "cecelia")
  
  # use environment.yml for type
  if (envType == "flow") {
    envFile <- system.file(
      file.path("py-env", "conda-env-flow.yml"),
      package = "cecelia")
    pyModulesFile <- system.file(
      file.path("py-env", "init-py-modules-flow.txt"),
      package = "cecelia")
  }
  
  # create conda environment
  # reticulate::install_miniconda()
  reticulate::conda_create(envName, environment = envFile)
  
  # install packages not in conda environment
  # TODO some of these did not work when included in the environment.yml
  pyModules <- readLines(pyModulesFile)
  pyModules <- pyModules[grepl(pattern = "^(?!#)", x = pyModules, perl = TRUE)]
  
  reticulate::conda_install(envname = envName, packages = pyModules, pip = TRUE)
}

#' @description Retrieve DL models
#' @param dlModels list of character for models. Defaults to
#' c("btrack", "mesmer.multiplex", "stardist.versatile2D", "ccia.fluo")
#' @export
cciaModels <- function(dlModels = c(
  "btrack", "mesmer.multiplex", "stardist.versatile2D", "ccia.fluo"
  )) {
  # create models directory
  modelsDir <- file.path(cciaPath(), "models")
  dir.create(modelsDir, showWarnings = FALSE)
  
  # Download ccia models form github repo
  download.file(
    url = "https://github.com/schienstockd/ceceliaModels/archive/refs/heads/master.zip",
    destfile = file.path(modelsDir, "cciaModels.zip"))
  unzip(zipfile = file.path(modelsDir, "cciaModels.zip"), exdir = modelsDir)
  unlink(file.path(modelsDir, "cciaModels.zip"))

  file.copy(file.path(modelsDir, "ceceliaModels-master", "btrackModels"),
            modelsDir, recursive = TRUE)
  file.copy(file.path(modelsDir, "ceceliaModels-master", "cellposeModels"),
            modelsDir, recursive = TRUE)
  unlink(file.path(modelsDir, "ceceliaModels-master"), recursive = TRUE)
  
  # Download Mesmer
  # TODO This is quite slow - am I allowed to push this on github?
  options(timeout = max(3600, getOption("timeout")))
  
  # https://github.com/vanvalenlab/deepcell-tf/blob/79fbf61187613daf830648927b82fc5e8d8d0d75/deepcell/applications/mesmer.py
  download.file(
    url = "https://deepcell-data.s3-us-west-1.amazonaws.com/saved-models/MultiplexSegmentation-9.tar.gz",
    destfile = file.path(modelsDir, "mesmerModels.tar.gz"))
  untar(tarfile = file.path(modelsDir, "mesmerModels.tar.gz"))
  # tar: could not chdir to './mesmerModels/MultiplexSegmentation'
  # exdir = )
  dir.create(file.path(modelsDir, "mesmerModels"), showWarnings = FALSE)
  file.copy("MultiplexSegmentation",
            file.path(modelsDir, "mesmerModels"),
            recursive = TRUE)
  unlink("MultiplexSegmentation", recursive = TRUE)
  unlink(file.path(modelsDir, "mesmerModels.tar.gz"))
  
  # Download Stardist
  # https://github.com/stardist/stardist/blob/810dec4727e8e8bf05bd9620f91a3a0dd70de289/stardist/models/__init__.py
  download.file(
    url = "https://github.com/stardist/stardist-models/releases/download/v0.1/python_2D_versatile_fluo.zip",
    destfile = file.path(modelsDir, "stardistModels.zip"))
  unzip(zipfile = file.path(modelsDir, "stardistModels.zip"),
        exdir = file.path(modelsDir, "stardistModels", "2D_versatile_fluo"))
  unlink(file.path(modelsDir, "stardistModels.zip"))
  
  # Cellpose will be loaded when running the model
}

#' @description Use cecelia working directory
#' @param path character to define working directory of cecelia
#' @export
cciaUse <- function(path, initConda = TRUE) {
  # check if there is a custom config
  customConf <- file.path(path, "custom.yml")
  
  if (file.exists(customConf)) {
    message("[CCIA] >> Add custom config")
    
    # include docker information in config
    x <- config::get(file = customConf)
    
    # replace volumes
    pkg.env$cfg$volumes <- x$volumes
    
    # copy everything else
    x <- x[names(x) != "volumes"]
    
    a <- unlist(x)
    b <- unlist(pkg.env$cfg)
    
    for (i in names(a)) {
      b[[i]] <- a[[i]]
    }
    
    # convert to numeric
    bRelist <- relist(b, pkg.env$cfg)
    
    # relist
    pkg.env$cfg <- list.as.numeric(bRelist)
  }
  
  # check if there is a docker config
  dockerConf <- file.path(path, "docker.yml")
  
  if (file.exists(dockerConf)) {
    # include docker information in config
    x <- config::get(file = dockerConf)
    pkg.env$cfg$docker <- x$docker
  }
  
  # init conda
  if (initConda == TRUE && !purrr::is_empty(pkg.env$cfg$python$conda$env)) {
    message(paste("[CCIA] >> Init conda", pkg.env$cfg$python$conda$env))
    
    reticulate::use_miniconda(pkg.env$cfg$python$conda$env, required = TRUE)
  }
  
  message("[CCIA] >> Source python files")
  
  # set working working directory
  os <- reticulate::import("os")
  os$chdir(system.file(".", package = "cecelia"))
  
  # source python files
  reticulate::source_python(
    system.file(file.path("py", "label_props_utils.py"), package = "cecelia"),
    envir = pkg.env
  )
}

#' @description Create app
#' @export
cciaCreateApp <- function() {
  # copy all files to project directory
  browser()
}

#' @description Run app with port
#' @export
cciaRunApp <- function(...) {
  shiny::runApp(system.file("app", package = "cecelia"), ...)
}