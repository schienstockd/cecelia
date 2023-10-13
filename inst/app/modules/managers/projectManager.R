# Project Manager
ProjectManager <- R6::R6Class(
  "ProjectManager",
  inherit = cecelia::ReactiveObject,
  
  ### public
  public = list(
    ## SETTINGS
    setProjectName = function(x, invalidate = TRUE) {
      private$projectName <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectType = function(x, invalidate = TRUE) {
      private$projectType <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectPath = function(x, invalidate = TRUE) {
      private$projectPath <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectUID = function(x, invalidate = TRUE) {
      private$projectUID <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCaddress = function(x, invalidate = TRUE) {
      private$projectHPCaddress <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCusername = function(x, invalidate = TRUE) {
      private$projectHPCusername <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCsshKeyfile = function(x, invalidate = TRUE) {
      private$projectHPCsshKeyfile <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCpartitionsCPU = function(x, invalidate = TRUE) {
      private$projectHPCpartitionsCPU <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCpartitionsGPU = function(x, invalidate = TRUE) {
      private$projectHPCpartitionsGPU <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCqosCPU = function(x, invalidate = TRUE) {
      private$projectHPCqosCPU <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCqosGPU = function(x, invalidate = TRUE) {
      private$projectHPCqosGPU <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCprojectCPU = function(x, invalidate = TRUE) {
      private$projectHPCprojectCPU <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCprojectGPU = function(x, invalidate = TRUE) {
      private$projectHPCprojectGPU <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCemail = function(x, invalidate = TRUE) {
      private$projectHPCemail <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCemailOnBegin = function(x, invalidate = TRUE) {
      private$projectHPCemailOnBegin <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCemailOnEnd = function(x, invalidate = TRUE) {
      private$projectHPCemailOnEnd <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectHPCemailOnFail = function(x, invalidate = TRUE) {
      private$projectHPCemailOnFail <- x
      private$invalidate(invalidate = invalidate)
    },
    
    ## VERSION
    setProjectVersionTp = function(x, invalidate = TRUE) {
      private$projectVersionTp <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectVersionID = function(x, invalidate = TRUE) {
      private$projectVersionID <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectVersionStateID = function(x, invalidate = TRUE) {
      private$projectVersionStateID <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectVersionComment = function(x, invalidate = TRUE) {
      private$projectVersionComment <- x
      private$invalidate(invalidate = invalidate)
    },
    
    ## Lab server
    setProjectLabServerSmbRemoteDir = function(x, invalidate = TRUE) {
      private$projectLabServerSmbRemoteDir <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectLabServerSmbRemoteAddon = function(x, invalidate = TRUE) {
      private$projectLabServerSmbRemoteAddon <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectLabServerSmbLocalMountDir = function(x, invalidate = TRUE) {
      private$projectLabServerSmbLocalMountDir <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectLabServerSmbUser = function(x, invalidate = TRUE) {
      private$projectLabServerSmbUser <- x
      private$invalidate(invalidate = invalidate)
    },
    
    setProjectLabServerSmbPwd = function(x, encrypt = TRUE, invalidate = TRUE) {
      # encyrpt password
      if (encrypt == TRUE) {
        x <- .cciaEncrypt(x)
      }
      
      private$handleProjectLabServerSmbPwd <- x
      private$invalidate(invalidate = invalidate)
    },
    
    ## SETTINGS
    getProjectName = function() {
      private$projectName
    },
    
    getProjectType = function() {
      private$projectType
    },
    
    getProjectPath = function(projectUID = NULL) {
      if (is.null(projectUID)) {
        projectUID <- self$getProjectUID()
      }
      
      # private$projectPath
      file.path(cciaConf()$dirs$projects, projectUID)
    },
    
    getProjectUID = function() {
      private$projectUID
    },
    
    getProjectHPCaddress = function() {
      private$projectHPCaddress
    },
    
    getProjectHPCusername = function() {
      private$projectHPCusername
    },
    
    getProjectHPCsshKeyfile = function() {
      private$projectHPCsshKeyfile
    },
    
    getProjectHPCpartitionsCPU = function() {
      private$projectHPCpartitionsCPU
    },
    
    getProjectHPCpartitionsGPU = function() {
      private$projectHPCpartitionsGPU
    },
    
    getProjectHPCqosCPU = function() {
      private$projectHPCqosCPU
    },
    
    getProjectHPCqosGPU = function() {
      private$projectHPCqosGPU
    },
    
    getProjectHPCprojectCPU = function() {
      private$projectHPCprojectCPU
    },
    
    getProjectHPCprojectGPU = function() {
      private$projectHPCprojectGPU
    },
    
    getProjectHPCemail = function() {
      private$projectHPCemail
    },
    
    getProjectHPCemailOnBegin = function() {
      private$projectHPCemailOnBegin
    },
    
    getProjectHPCemailOnEnd = function() {
      private$projectHPCemailOnEnd
    },
    
    getProjectHPCemailOnFail = function() {
      private$projectHPCemailOnFail
    },
    
    # check whether HPC can be used or not
    useHPC = function(reset = FALSE, invalidate = TRUE) {
      # reset value
      if (reset == TRUE) {
        private$projectHPCuseHPC <- NULL
      }
      
      # check whether to check for HPC
      if (cciaConf()$hpc$useHPC == FALSE) {
        private$projectHPCuseHPC <- FALSE
      }
      
      # check connection
      if (is.null(private$projectHPCuseHPC)) {
        sshUsername <- self$getProjectHPCusername()
        sshAddress <- self$getProjectHPCaddress()
        sshKeyfile <- self$getProjectHPCsshKeyfile()
        sshConf <- list(sshUsername, sshAddress)
        
        # are HPC settings set?
        if (any(unlist(lapply(sshConf, is.null)))) {
          private$projectHPCuseHPC <- FALSE
        } else {
          # keyfile
          sshUtils <- SshUtils$new(
            username = sshUsername,
            address = sshAddress,
            keyfile = sshKeyfile
            )
          
          # check HPC connection
          if (sshUtils$testConnection() == TRUE) {
            private$projectHPCuseHPC <- TRUE
          } else {
            private$projectHPCuseHPC <- FALSE
          }
          
          # invalidate
          private$invalidate(invalidate = invalidate)
        }
      }
      
      private$projectHPCuseHPC
    },
    
    ## VERSION
    getProjectVersionTp = function() {
      private$projectVersionTp
    },
    
    getProjectVersionID = function() {
      private$projectVersionID
    },
    
    getProjectVersionStateID = function() {
      private$projectVersionStateID
    },
    
    getProjectVersionComment = function() {
      private$projectVersionComment
    },
    
    ## Lab server
    getProjectLabServerSmbRemoteDir = function(x) {
      private$projectLabServerSmbRemoteDir
    },
    
    getProjectLabServerSmbRemoteAddon = function(x) {
      private$projectLabServerSmbRemoteAddon
    },
    
    getProjectLabServerSmbLocalMountDir = function(x) {
      private$projectLabServerSmbLocalMountDir
    },
    
    getProjectLabServerSmbUser = function(x) {
      private$projectLabServerSmbUser
    },
    
    # this will not be saved
    projectLabServerSmbPwd = function(x, decrypt = FALSE, forBash = FALSE) {
      retVal <- private$handleProjectLabServerSmbPwd
      
      # decrypt password
      if (decrypt == TRUE) {
        retVal <- .cciaDecrypt(retVal)
      }
      
      # prep for bash output
      if (forBash == TRUE) {
        retVal <- .prepForBash(retVal)
      }
      
      retVal
    },
    
    # test connection to lab server
    testLabServerConnection = function() {
      conOK <- TRUE
      
      # test connection
      sshCon <- SshUtils$new(projectManager = self$reactive())
      
      smbCmd <- sprintf(
        "echo $'%s' | smbclient %s -U %s -c 'ls'",
        self$projectLabServerSmbPwd(decrypt = TRUE, forBash = TRUE),
        self$getProjectLabServerSmbRemoteDir(),
        self$getProjectLabServerSmbUser()
      )
      
      # exec
      retVal <- sshCon$sshExecute(smbCmd)
      
      if ("status" %in% names(attributes(retVal))){
        conOK <- FALSE
      } 
      
      conOK
    },
    
    ## DATA
    # get persistent object directory
    persistentObjectDirectory = function(
      uID = NULL, ccidFile = FALSE, version = NULL) {
      # get version
      if (is.null(version)) {
        version <- self$getProjectVersionID()
      }
      
      objDir <- file.path(
        self$getProjectPath(),
        cciaConf()$dirs$analysis,
        version)
      
      # add uID
      if (!is.null(uID)){
        objDir <- file.path(
          objDir, uID
        )
      }
      
      # create
      if (length(objDir) > 0) {
        dir.create(
          objDir, showWarnings = FALSE, recursive = TRUE)
      }
      
      # add ccid file
      if (ccidFile == TRUE){
        objDir <- file.path(
          objDir, cecelia:::CCID_STATE_FILE
        )
      }
      
      objDir
    },
    
    # generate uID
    genUID = function() {
      genUID(as.numeric(cciaConf()$images$lenUID))
    },
    
    # return HPC path
    hpcProjectDirectory = function(version = NULL, projectUID = NULL) {
      # get a specific version
      if (is.null(version)) {
        version <- self$getProjectVersionID()
      }
      
      # get a specific project
      if (is.null(projectUID)) {
        projectUID <- self$getProjectUID()
      }
      
      path <- paste(
        cciaConf()$hpc$dirs$base,
        cciaConf()$hpc$dirs$users,
        # include username
        self$getProjectHPCusername(),
        projectUID,
        cciaConf()$hpc$dirs$analysis,
        version,
        sep = "/"
      )
      
      path
    },
    
    # return HPC path
    persistentObjectHPCDirectory = function(
      uID = NULL, version = NULL) {
      path <- paste(
        self$hpcProjectDirectory(version),
        sep = "/"
      )
      
      # add uID
      if (!is.null(uID)){
        path <- paste(
          path, uID,
          sep = "/"
        )
      }
      
      path
    },
    
    ## VERSION
    # load a project version
    loadProjectVersion = function(versionID = NULL) {
      versionInfo <- private$getProjectVersionInfo(versionID)
      
      # restore bookmark
      private$session$sendCustomMessage(
        'restoreBookmark',
        buildShinyBookmarkURL(private$session, versionInfo$stateID))
    },
    
    # export project
    exportProject = function(exportDir, bookmarkStateDir = NULL) {
      # define paths
      projectFile <- file.path(self$getProjectPath(), "project.csv")
      bookmarksFile <- file.path(self$getProjectPath(), "shinyBookmarks.csv")
      bookmarkDir <- file.path(self$getProjectPath(), "shiny_bookmarks")
      
      # save project table entries
      projectInfo <- self$projectTable(onlyProject = TRUE)
      bookmarkInfo <- self$projectVersionTable()
      
      write.csv(projectInfo, projectFile, row.names = FALSE)
      write.csv(bookmarkInfo, bookmarksFile, row.names = FALSE)
      
      # save bookmarks
      bookmarkDirs <- sapply(
        bookmarkInfo$stateID,
        function(x) getShinyBookmarkStatePath(x, bookmarkStateDir = bookmarkStateDir)
      )
      
      dir.create(bookmarkDir)
      file.copy(bookmarkDirs, bookmarkDir, recursive = TRUE)
      
      # tar everything together and copy
      # go to directory and then tar
      wd <- getwd()
      setwd(dirname(self$getProjectPath()))
      
      tar(tarfile = file.path(exportDir, paste0(self$getProjectUID(), ".tar")),
          files = self$getProjectUID())
      
      # switch back
      setwd(wd)
      
      # remove temporary files from project directory
      unlink(bookmarkDir, recursive = TRUE)
      unlink(bookmarksFile, recursive = TRUE)
    },
    
    # import project
    importProject = function(importFile, bookmarkStateDir = NULL, isDir = FALSE) {
      # get file paths
      archivePath <- file.path(cciaConf()$dirs$projects, basename(importFile))
      
      # copy to projects
      file.copy(importFile, cciaConf()$dirs$projects, recursive = TRUE)
      
      # get project ID and untar
      # pID <- basename(untar(archivePath, exdir = cciaConf()$dirs$projects, list = TRUE)$Name[[1]])
      # TODO this assumes the file is named as the project ID
      pID <- tools::file_path_sans_ext(basename(archivePath))
      pPath <- file.path(cciaConf()$dirs$projects, pID)
      bookmarksPath <- file.path(pPath, "shiny_bookmarks")
      projectFile <- file.path(pPath, "project.csv")
      bookmarkFile <- file.path(pPath, "shinyBookmarks.csv")
      
      # check whether to unpack
      if (tools::file_ext(basename(archivePath)) %in% c("tar", "zip")) {
        if (tools::file_ext(archivePath) == "tar")
          untar(archivePath, exdir = cciaConf()$dirs$projects)
        else if (tools::file_ext(archivePath) == "zip")
          unzip(archivePath, exdir = cciaConf()$dirs$projects)
        
        # delete tar
        unlink(archivePath)
      } 
      
      # TODO get a shiny bookmark url
      # otherwise I do not know where the bookmarks are saved
      # that means, a user has to create a dummy project
      # if they only want to import another project
      # maybe that could be done automatically .. ?
      # or/ you should find out how shiny determines
      # the bookmark directories for individual apps
      firstPID <- self$projectTable()$projectUID[[1]]
      firstStateID <- self$projectVersionTable(projectUID = firstPID)$stateID[[1]]
      
      # copy shiny bookmarks
      shinyBookmarkStateDir <- dirname(
        getShinyBookmarkStatePath(firstStateID, bookmarkStateDir = bookmarkStateDir)
      )
      
      bookmarksToCopy <- list.files(bookmarksPath,
                                    include.dirs = TRUE, full.names = TRUE)
      file.copy(bookmarksToCopy, shinyBookmarkStateDir, recursive = TRUE)
      
      # delete bookmarks
      unlink(bookmarksPath, recursive = TRUE)
      
      # copy database information
      projectInfo <- read.csv(projectFile)[1,]
      bookmarkInfo <- read.csv(bookmarkFile)
      
      # add information to db
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # go through project info
      valNames <- c("projectUID", "name", "type", "tp")
      sqlQuery <- sprintf(
        "INSERT INTO %s ('%s') VALUES (%s)",
        dbTABLE_PROJECTS,
        paste(valNames, collapse = "', '"),
        paste(rep("?", length(valNames)), collapse = ", ")
      )
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        projectInfo$projectUID,
        projectInfo$name,
        projectInfo$type,
        projectInfo$tp
      )
      
      dbExecute(dbConn, dbQuery)
      
      # go through bookmark info
      for (i in seq(nrow(bookmarkInfo))) {
        x <- bookmarkInfo[i,]
        
        valNames <- c("projectUID", "version", "tp", "stateID", "comment")
        sqlQuery <- sprintf(
          "INSERT INTO ? ('%s') VALUES (%s)",
          paste(valNames, collapse = "', '"),
          paste(rep("?", length(valNames)), collapse = ", ")
        )
        
        dbQuery <- DBI::sqlInterpolate(
          dbConn, sqlQuery,
          dbTABLE_PROJECT_VERSIONS,
          projectInfo$projectUID,
          x$version,
          x$tp,
          x$stateID,
          x$comment
        )
        
        dbExecute(dbConn, dbQuery)
      }
      
      # delete infos
      unlink(projectFile)
      unlink(bookmarkFile)
      
      # close connection
      DBI::dbDisconnect(dbConn)
    },
    
    # delete a project 
    deleteProject = function(projectUID = NULL) {
      # set to current project
      if (is.null(projectUID)) {
        projectUID <- self$getProjectUID()
      }
      
      # get project information
      versionInfos <- self$projectVersionTable(
        projectUID = projectUID)
      
      # connect to db
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # remove versions
      sqlQuery <- 'DELETE FROM ?table WHERE projectUID == ?projectUID'
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        table = dbTABLE_PROJECT_VERSIONS,
        projectUID = projectUID)
      
      dbExecute(dbConn, dbQuery)
      
      # remove project
      sqlQuery <- 'DELETE FROM ?table WHERE projectUID == ?projectUID'
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        table = dbTABLE_PROJECTS,
        projectUID = projectUID)
      
      dbExecute(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
      
      # remove shiny bookmarks
      for(i in 1:nrow(versionInfos)) {
        unlinkShinyBookmark(versionInfos[i,]$stateID)
      }
        
      # remove project directory
      unlink(
        self$getProjectPath(projectUID = projectUID),
        recursive = TRUE
      )
    },
    
    # check that a project with this name not already exitsts
    isProjectNameUnique = function(projectName) {
      nameUnique <- TRUE
      
      # get connection
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # was a project table created?
      sqlQuery <- "SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = ?table"
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        table = dbTABLE_PROJECTS)
      
      dbRes <- DBI::dbGetQuery(dbConn, dbQuery)
      
      if (dbRes$count > 0) {
        sqlQuery <- 'SELECT COUNT(*) AS count FROM ?table WHERE name == ?name LIMIT 1'
        dbQuery <- DBI::sqlInterpolate(
          dbConn, sqlQuery,
          table = dbTABLE_PROJECTS,
          name = projectName)
        
        dbRes <- DBI::dbGetQuery(dbConn, dbQuery)
        
        nameUnique <- !(dbRes$count > 0)
      }
      
      # close connection
      DBI::dbDisconnect(dbConn)
      
      nameUnique
    },
    
    # create a project 
    createProject = function() {
      # create a new project
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # create project table
      dbQuery <- sprintf(
        paste0(
          'CREATE TABLE IF NOT EXISTS "%s" (',
          '"projectUID" TEXT,',
          '"name"	TEXT,',
          '"type" TEXT,',
          '"tp" DATETIME,',
          'PRIMARY KEY("projectUID")',
          ');'),
        dbTABLE_PROJECTS
      )
      
      dbExecute(dbConn, dbQuery)
      
      # create version table
      dbQuery <- sprintf(
        paste0(
          'CREATE TABLE IF NOT EXISTS "%s" (',
          '"projectUID" TEXT,',
          '"version"	INTEGER,',
          '"tp" DATETIME,',
          '"stateID" TEXT,',
          '"comment" TEXT,',
          'PRIMARY KEY("projectUID", "version")',
          ');'),
        dbTABLE_PROJECT_VERSIONS
      )
      
      dbExecute(dbConn, dbQuery)
      
      # add information to db
      valNames <- c("projectUID", "name", "type", "tp")
      sqlQuery <- sprintf(
        "INSERT INTO %s ('%s') VALUES (%s)",
        dbTABLE_PROJECTS,
        paste(valNames, collapse = "', '"),
        paste(rep("?", length(valNames)), collapse = ", ")
      )
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        self$getProjectUID(),
        self$getProjectName(),
        self$getProjectType(),
        as.character(Sys.time())
      )
      
      dbExecute(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
    },
    
    # delete a project version
    deleteProjectVersion = function(versionID, projectUID = NULL) {
      # do not delete all versions
      numVersions <- self$projectVersionTable(TRUE)
      
      if (numVersions > 1) {
        # get version information
        versionInfo <- private$getProjectVersionInfo(versionID)
        
        # connect to db
        dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
        
        sqlQuery <- 'DELETE FROM ?table WHERE projectUID == ?projectUID AND version == ?version'
        
        dbQuery <- DBI::sqlInterpolate(
          dbConn, sqlQuery,
          table = dbTABLE_PROJECT_VERSIONS,
          projectUID = self$getProjectUID(),
          version = versionID
        )
        
        dbExecute(dbConn, dbQuery)
        
        # remove shiny bookmark
        unlinkShinyBookmark(
          versionInfo$stateID)
        
        # close connection
        DBI::dbDisconnect(dbConn)
        
        # delete content
        self$deleteVersionContent(versionID)
      }
    },
    
    # create a project version
    createProjectVersion = function() {
      # get number of versions
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # get last version if no version given
      sqlQuery <- 'SELECT version FROM ?table WHERE projectUID == ?projectUID ORDER BY version DESC LIMIT 1'
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        table = dbTABLE_PROJECT_VERSIONS,
        projectUID = self$getProjectUID()
      )
      
      dbRes <- DBI::dbGetQuery(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
      
      # set new version
      prevVersion <- as.numeric(dbRes[1, "version"])
      
      prevVersion + 1
    },
    
    # check whether the version exists
    existProjectVersion = function(versionID) {
      # connect to db
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # get last version if no version given
      sqlQuery <- 'SELECT version FROM ?table WHERE projectUID == ?projectUID AND version == ?version'

      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        table = dbTABLE_PROJECT_VERSIONS,
        projectUID = self$getProjectUID(),
        version = versionID
      )
      
      dbRes <- DBI::dbGetQuery(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
      
      # check
      nrow(dbRes) > 0
    },
    
    # do project bookmark
    doProjectBookmark = function(unlinkPrevBookmark = TRUE){
      # remove old bookmark
      if (unlinkPrevBookmark == TRUE) {
        unlinkShinyBookmark(
          self$getProjectVersionStateID())
      }
      
      # save analysis and other files
      # ...
      
      # bookmark
      private$session$doBookmark()
    },
    
    # get all projects
    projectTable = function(countOnly = FALSE, onlyProject = FALSE) {
      # get versions
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # get version info
      if (countOnly == TRUE){
        sqlQuery <- 'SELECT COUNT(*) AS count FROM ?table'
      } else {
        if (onlyProject == TRUE) {
          sqlQuery <- 'SELECT * FROM ?table WHERE projectUID == ?projectUID ORDER BY date(tp) DESC'
          
          dbQuery <- DBI::sqlInterpolate(
            dbConn, sqlQuery,
            table = dbTABLE_PROJECTS,
            projectUID = self$getProjectUID()
          )
        } else {
          sqlQuery <- 'SELECT * FROM ?table ORDER BY date(tp) DESC'
          
          dbQuery <- DBI::sqlInterpolate(
            dbConn, sqlQuery,
            table = dbTABLE_PROJECTS
          )
        }
      }
      
      dbRes <- DBI::dbGetQuery(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
      
      # convert to dataframe
      dbRes <- as.data.frame(dbRes)
      
      if (countOnly == TRUE){
        dbRes <- dbRes[1,1]
      }
      
      return(dbRes)
    },
    
    # get all project versions
    projectVersionTable = function(
      countOnly = FALSE, projectUID = NULL, excludeProjectUID = TRUE){
      if (is.null(projectUID)) {
        projectUID <- self$getProjectUID()
      }
      
      # get versions
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # get version info
      if (countOnly == TRUE){
        sqlQuery <- 'SELECT COUNT(*) AS count FROM ?table WHERE projectUID == ?projectUID'
      } else {
        sqlQuery <- 'SELECT * FROM ?table WHERE projectUID == ?projectUID ORDER BY version DESC'
      }
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        table = dbTABLE_PROJECT_VERSIONS,
        projectUID = projectUID
      )
      
      dbRes <- DBI::dbGetQuery(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
      
      # convert to dataframe
      dbRes <- as.data.frame(dbRes)
      
      if (countOnly == TRUE){
        dbRes <- dbRes[1,1]
      }
      
      # exclude project id
      if (!is.null(nrow(dbRes))) {
        if (excludeProjectUID == TRUE) {
          dbRes <- dbRes[, names(dbRes) != "projectUID"]
        }
      }
      
      return(dbRes)
    },
    
    # update project table
    updateProjectTable = function() {
      valNames <- c("name", "tp")
      sqlQuery <- sprintf(
        'UPDATE ? SET %s WHERE projectUID == ?',
        paste(
          paste(sprintf(
            "%s = %s", valNames,
            rep("?", length(valNames))
          )), collapse = ", "
        )
      )
      
      # connect to db
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        dbTABLE_PROJECTS,
        self$getProjectName(),
        as.character(Sys.time()),
        self$getProjectUID()
      )

      dbExecute(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
    },
    
    # update project version table
    updateProjectVersionTable = function(stateID) {
      # was a new version created?
      if (self$existProjectVersion(self$getProjectVersionID())) {
        valNames <- c("tp", "stateID")
        sqlQuery <- sprintf(
          'UPDATE ? SET %s WHERE projectUID == ? AND version == ?',
          paste(
            paste(sprintf(
              "%s = %s", valNames,
              rep("?", length(valNames))
            )), collapse = ", "
          )
        )
        
        # connect to db
        dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
  
        dbQuery <- DBI::sqlInterpolate(
          dbConn, sqlQuery,
          dbTABLE_PROJECT_VERSIONS,
          self$getProjectVersionTp(),
          stateID,
          self$getProjectUID(),
          self$getProjectVersionID()
        )
      } else {
        # is this the first version?
        numVersions <- self$projectVersionTable(TRUE)
        
        if (numVersions == 0) {
          versionComment <- "initial commit"
        } else {
          versionComment <- self$getProjectVersionComment()
        }
        
        valNames <- c("projectUID", "version", "tp", "stateID", "comment")
        sqlQuery <- sprintf(
          "INSERT INTO ? ('%s') VALUES (%s)",
          paste(valNames, collapse = "', '"),
          paste(rep("?", length(valNames)), collapse = ", ")
        )
        
        # connect to db
        dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
        
        dbQuery <- DBI::sqlInterpolate(
          dbConn, sqlQuery,
          dbTABLE_PROJECT_VERSIONS,
          self$getProjectUID(),
          self$getProjectVersionID(),
          self$getProjectVersionTp(),
          stateID,
          versionComment
        )
      }
      
      dbExecute(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
    },
    
    # update settings
    updateStateID = function() {
      # get state ID
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      sqlQuery <- 'SELECT * FROM ?table WHERE projectUID == ?projectUID AND version == ?version'
      
      dbQuery <- DBI::sqlInterpolate(
        dbConn, sqlQuery,
        table = dbTABLE_PROJECT_VERSIONS,
        projectUID = self$getProjectUID(),
        version = self$getProjectVersionID()
      )
      
      # submit
      dbRes <- DBI::dbGetQuery(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
      
      # set state ID
      self$setProjectVersionStateID(
        dbRes[1, "stateID"])
    },
    
    # copy version content
    copyVersionContent = function(prevVersionID, nextVersionID) {
      # get directories
      prevDir <- self$persistentObjectDirectory(version = prevVersionID)
      nextDir <- self$persistentObjectDirectory(version = nextVersionID)
      
      # create directory
      dir.create(nextDir, showWarnings = FALSE)
      
      # copy
      file.copy(
        list.files(prevDir, full.names = TRUE),
        to = nextDir, 
        overwrite = TRUE, recursive = TRUE, 
        copy.mode = TRUE)
    },
    
    # copy HPC version content
    copyHPCVersionContent = function(prevVersionID, nextVersionID) {
      # get directories
      prevDir <- self$hpcProjectDirectory(version = prevVersionID)
      nextDir <- self$hpcProjectDirectory(version = nextVersionID)
      
      # execute
      sshUtil <- SshUtils$new(projectManager = self$reactive())
      handleSystem(sshUtil$sshExecute(
        paste(
          sprintf("mkdir %s", nextDir),
          sprintf(
            "cp -R %s/* %s",
            prevDir, nextDir),
          sep = ";"
        )
      ), silent = TRUE)
    },
    
    # delete version content
    deleteVersionContent = function(versionID) {
      unlink(
        self$persistentObjectDirectory(version = versionID),
        recursive = TRUE
      )
    },
    
    # delete HPC project content
    deleteHPCProjectContent = function(projectUID = NULL) {
      if (is.null(projectUID)) {
        projectUID <- self$getProject
      }
      
      sshUtil <- SshUtils$new(projectManager = self$reactive())
      handleSystem(sshUtil$sshExecute(
        sprintf(
          "rm -fr \"%s\"",
          self$hpcProjectDirectory(projectUID = projectUID)
          )
      ), silent = TRUE)
    },
    
    # delete HPC version content
    deleteHPCVersionContent = function(versionID) {
      sshUtil <- SshUtils$new(projectManager = self$reactive())
      handleSystem(sshUtil$sshExecute(
        sprintf(
          "rm -fr \"%s\"",
          self$hpcProjectDirectory(version = versionID)
          )
      ), silent = TRUE)
    },
    
    # delete HPC persisten object directory
    deleteHPCpersistentObjectDirectory = function(uID, removeZero = FALSE) {
      sshUtil <- SshUtils$new(projectManager = self$reactive())
      
      # zero?
      if (removeZero == TRUE) {
        handleSystem(sshUtil$sshExecute(
          sprintf(
            "rm -fr \"%s\"",
            self$persistentObjectHPCDirectory(uID, version = 0)
          )
        ), silent = TRUE)
      }
      
      # task directory
      handleSystem(sshUtil$sshExecute(
        sprintf(
          "rm -fr \"%s\"",
          self$persistentObjectHPCDirectory(uID)
        )
      ), silent = TRUE)
    }
  ),
  
  ### private
  private = list(
    ## SETTINGS
    # variables
    projectName = NULL,
    projectType = NULL,
    projectPath = NULL,
    projectUID = NULL,
    
    # HPC
    projectHPCuseHPC = NULL,
    projectHPCaddress = NULL,
    projectHPCusername = NULL,
    projectHPCsshKeyfile = NULL,
    projectHPCpartitionsCPU = NULL,
    projectHPCpartitionsGPU = NULL,
    projectHPCqosCPU = NULL,
    projectHPCqosGPU = NULL,
    projectHPCprojectCPU = NULL,
    projectHPCprojectGPU = NULL,
    projectHPCemail = NULL,
    projectHPCemailOnBegin = NULL,
    projectHPCemailOnEnd = NULL,
    projectHPCemailOnFail = NULL,
    
    ## VERSION
    projectVersionTp = NULL,
    projectVersionID = NULL,
    projectVersionStateID = NULL,
    projectVersionComment = NULL,
    
    # lab server 
    projectLabServerSmbRemoteDir = NULL,
    projectLabServerSmbRemoteAddon = NULL,
    projectLabServerSmbLocalMountDir = NULL,
    projectLabServerSmbUser = NULL,
    handleProjectLabServerSmbPwd = NULL,
    
    # get database file
    dbFile = function() {
      # this will give the path to the project directory
      # file.path(
      #   self$getProjectPath(),
      #   CCIPDB)
      
      suppressWarnings({
        dir.create(cciaConf()$dirs$database)
      })
      
      # this will give the path to the package directory
      file.path(cciaConf()$dirs$database, CCIPDB)
    },
    
    # get information about a project version
    getProjectVersionInfo = function(versionID = NULL) {
      # load last state of project
      
      # connect to db
      dbConn <- DBI::dbConnect(RSQLite::SQLite(), private$dbFile())
      
      # get last version if no version given
      if (is.null(versionID)) {
        sqlQuery <- 'SELECT * FROM ?table WHERE projectUID == ?projectUID ORDER BY version DESC LIMIT 1'

        dbQuery <- DBI::sqlInterpolate(
          dbConn, sqlQuery,
          table = dbTABLE_PROJECT_VERSIONS,
          projectUID = self$getProjectUID()
        )
      } else {
        sqlQuery <- 'SELECT * FROM ?table WHERE projectUID == ?projectUID AND version == ?version LIMIT 1'
        
        dbQuery <- DBI::sqlInterpolate(
          dbConn, sqlQuery,
          table = dbTABLE_PROJECT_VERSIONS,
          projectUID = self$getProjectUID(),
          version = versionID
        )
      }
      
      dbRes <- DBI::dbGetQuery(dbConn, dbQuery)
      
      # close connection
      DBI::dbDisconnect(dbConn)
      
      return(as.data.frame(dbRes)[1,])
    }
  )
)
