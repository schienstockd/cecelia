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

#' @description Napari utils
#' @export
cciaNapariUtils <- function() {
  cciaEnv()$napariUtils
}

#' @description Setup cecelia working directory
#' @param path character to define working directory of cecelia
#' @export
cciaSetup <- function(path = "~/cecelia") {
  # set path in environment
  pkg.env$path <- path
  
  # create directory if not present
  dir.create(path, showWarnings = FALSE)
  
  # copy in custom.yml to be modified
  file.copy(system.file("custom.yml", package = "cecelia"), file.path(path))
}

#' @description Create conda environment for cecelia
#' @param envName character for environment name
#' @param envType character for environment type. Any of c("image", "flow")
#' @export
cciaCondaCreate <- function(envName = "r-cecelia-env", envType = "image",
                            rebuild = FALSE) {
  envFile <- system.file(
    file.path("py-env", "conda-env.yml"),
    package = "cecelia")
  
  pyModulesFile <- system.file(
    file.path("py-env", "init-py-modules-image.txt"),
    package = "cecelia")
  
  # use environment.yml for type
  if (envType == "flow") {
    pyModulesFile <- system.file(
      file.path("py-env", "init-py-modules-flow.txt"),
      package = "cecelia")
  } else if (envType == "image-nogui") {
    pyModulesFile <- system.file(
      file.path("py-env", "init-py-modules-image-nogui.txt"),
      package = "cecelia")
  }
  
  # create conda environment
  envPresent <- envName %in% reticulate::conda_list()$name
  
  if (envPresent == FALSE || rebuild == TRUE) {
    # reticulate::install_miniconda()
    reticulate::conda_remove(envName)
    reticulate::conda_create(envName, environment = envFile)
  } else {
    # TODO only install dependencies?
    # reticulate::conda_install()
  }
  
  # install packages not in conda environment
  # TODO some of these did not work when included in the environment.yml
  pyModules <- readLines(pyModulesFile)
  pyModules <- pyModules[grepl(pattern = "^(?!#)", x = pyModules, perl = TRUE)]
  
  # create pip options
  pipOptions <- c(
    # "--user",
    "-U",
    # for A100 support?
    # https://pytorch.org/get-started/locally/
    "--extra-index-url https://download.pytorch.org/whl/cu116"
  )
  
  # some install fail on Apple M1
  if (envType %in% c("image", "image-nogui")) {
    reticulate::conda_install(
      envname = envName, packages = c("openjdk"),
      channel = c("anaconda")
    )
    
    reticulate::conda_install(
      envname = envName, packages = c("python-javabridge"),
      pip = TRUE, pip_options = pipOptions
    )
    
    # cvxopt
    reticulate::conda_install(
      envname = envName, packages = c("cvxopt", "numcodecs"),
      channel = c("conda-forge")
    )
  }
  
  reticulate::conda_install(
    envname = envName, packages = pyModules,
    # channel = c("conda-forge", "anaconda")
    pip = TRUE, pip_options = pipOptions
    )
    
  # install OME bioformats
  if (envType %in% c("image", "image-nogui")) {
    reticulate::conda_install(
      envname = envName, packages = c("bioformats2raw"), channel = "ome")
    reticulate::conda_install(
      envname = envName, packages = c("bftools"), channel = "bioconda")
  }
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
#' @param initConda boolean to init conda environment
#' @param initJupyter boolean to init jupyter server
#' @param jupyterLibDir character to define library for jupyter python source files
#' @param sourceConda boolean to source conda files
#' @param projectsDir character to define project directory of cecelia
#' @export
cciaUse <- function(path = "~/cecelia", initConda = TRUE, initJupyter = FALSE,
                    jupyterConnectionFile = NULL, jupyterLibDir = NULL,
                    sourceConda = TRUE, projectsDir = NULL, localPath = TRUE) {
  # set path in environment
  pkg.env$path <- path
  
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
  
  # add specified project directory
  if (!is.null(projectsDir)) {
    pkg.env$cfg$dirs$projects <- projectsDir
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
    
    reticulate::use_condaenv(pkg.env$cfg$python$conda$env, required = TRUE)
  
    # init jupyter
    if (initJupyter == TRUE) {
      if (is.null(jupyterConnectionFile))
        pkg.env$napariUtils <- NapariUtils$new(useConnectionFile = FALSE, libDir = jupyterLibDir)
      else
        pkg.env$napariUtils <- NapariUtils$new(connectionFile = jupyterConnectionFile, libDir = jupyterLibDir)
    }
    
    # set working working directory
    os <- reticulate::import("os")
    os$chdir(system.file(package = "cecelia"))
  }
  
  if (sourceConda == TRUE) {
    message("[CCIA] >> Source python files")
    
    # source python files
    reticulate::source_python(
      system.file(file.path("py", "label_props_utils.py"), package = "cecelia"),
      envir = pkg.env
    )
  }
}

#' @description Create app
#' @export
cciaCreateApp <- function() {
  # copy all files to project directory
  copyPrevious <- FALSE
  
  # copy previous app
  if (file.exists(file.path(cciaPath(), "app"))) {
    copyPrevious <- TRUE
    
    # remove previous bak
    if (file.exists(file.path(cciaPath(), "app.bak"))) {
      unlink(file.path(cciaPath(), "app.bak"), recursive = TRUE)
    }
    
    # copy to bak
    file.rename(
      file.path(cciaPath(), "app"), file.path(cciaPath(), "app.bak")
    )
    
    # remove app
    unlink(file.path(cciaPath(), "app"), recursive = TRUE)
  }
  
  # copy new app
  file.copy(
    system.file("app", package = "cecelia"), file.path(cciaPath()),
    recursive = TRUE
  )
  
  # copy config, db and shiny_bookmarks
  if (copyPrevious == TRUE) {
    if (file.exists(file.path(cciaPath(), "app.bak", "docker.yml"))) {
      file.copy(
        file.path(cciaPath(), "app.bak", "docker.yml"),
        file.path(cciaPath(), "app", "docker.yml")
      )
    }
    file.copy(
      file.path(cciaPath(), "app.bak", "custom.yml"),
      file.path(cciaPath(), "app", "custom.yml")
    )
    file.copy(
      file.path(cciaPath(), "app.bak", "db"),
      file.path(cciaPath(), "app"),
      recursive = TRUE
    )
    file.copy(
      file.path(cciaPath(), "app.bak", "shiny_bookmarks"),
      file.path(cciaPath(), "app"),
      recursive = TRUE
    )
  }
}

#' @description Run app with port
#' @param localPath boolean to use local path
#' @export
cciaRunApp <- function(localPath = TRUE, ...) {
  if (localPath == TRUE)
    shiny::runApp(file.path(cciaPath(), "app"), ...)
  else
    shiny::runApp(system.file("app", package = "cecelia"), ...)
}

#' @description Install app requirements
#' @param ncpus number of parallel processes
#' @param ... passed to install.packages
#' @export
cciaAppRequirements <- function(ncpus = 4, ...) {
  # use binary for rgl
  # https://stackoverflow.com/q/51289395
  # options(pkgType="binary")
  
  # read packages to install
  packagesToInstall <- readLines(
    system.file("r-requirements.txt", package = "cecelia"))

  install.packages(
    packagesToInstall,
    dependencies = TRUE, Ncpus = ncpus, ...)
  
  # install rasterly
  # TODO maintained alternative?
  remotes::install_github("plotly/rasterly", ...)
}

#' @description Install bioconductor requirements
#' @param ncpus number of parallel processes
#' @param ... passed to BiocManager::install
#' @export
cciaBiocRequirements <- function(ncpus = 4, ...) {
  # for R 4.2
  if (R.version$major == 4 && stringr::str_detect(R.version$minor, "^2\\."))
    BiocManager::install(version = '3.15', ...)
  if (R.version$major == 4 && stringr::str_detect(R.version$minor, "^1\\."))
    BiocManager::install(version = '3.14', ...)
  
  # downgrade reticulate; should be done in DESCRIPTION?
  # The following is not solved
  # https://github.com/rstudio/reticulate/issues/1155
  # remotes::install_version("reticulate", "1.22", repos = "https://cloud.r-project.org")
  
  # BiocManager::install(
  #   # c("openCyto", "ggcyto", "flowCore", "flowWorkspace", "aoles/RBioFormats", "EBImage")
  #   # c("S4Vectors", "openCyto", "ggcyto", "flowCore", "flowWorkspace"), Ncpus = ncpus, ...)
  #   # c("S4Vectors", "ggcyto", "flowCore", "flowWorkspace"), Ncpus = ncpus, ...)
  #   c("S4Vectors"), Ncpus = ncpus, ...)
  
  # install protobuf separately
  # remotes::install_github("rglab/RProtoBufLib", upgrade = "never", ...)
  remotes::install_github("rglab/RProtoBufLib", upgrade = "always", ...)
  
  # install separately
  remotes::install_github("rglab/cytolib", upgrade = "never", ...)
  remotes::install_github("rglab/flowCore", upgrade = "never", ...)
  remotes::install_github("rglab/flowWorkspace", upgrade = "never", ...)
  remotes::install_github("rglab/ggcyto", upgrade = "never", ...)
}