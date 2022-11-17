ApplyAutospill <- R6::R6Class(
  "ApplyAutospill",
  inherit = GatePopulations,
  
  private = list(
    # create gating set for compensation population
    createGatingSet = function(fs, gateIDs, uID, valueName) {
      # get DT
      fsDT <- .flowFortifyGs(fs)
      
      # get coordinates from gates events
      # TODO this assumes gating in FSC-A SSC-A
      gateDT <- fsDT[gateIDs,]
      gateAxis <- c("FSC-A", "SSC-A")
      gateCoords <- gateDT[
        chull(as.matrix(gateDT[, ..gateAxis])), ..gateAxis
      ] %>% dplyr::arrange(dplyr::desc(dplyr::row_number()))
      
      # create gating set and add gate
      gateObj <- self$initCciaObject(uID)
      
      # create gs
      gs <- .flowCreateGatingSet(fs, ffNames = gateObj$getUID())
      
      # create gate
      flist <- .flowPolygonGate(gateCoords, gateAxis[[1]], gateAxis[[2]])
      names(flist) <- gateObj$getUID()
      flowWorkspace::gs_pop_add(gs, flist, name = "comp")
      flowWorkspace::recompute(gs)
      
      # save gs
      gsPath <- file.path(
        gateObj$persistentObjectDirectory(),
        taskDirFiles("data", paste0(valueName, cciaConf()$files$ext$gatingSet))
      )
      
      # remove gating set before saving
      unlink(gsPath, recursive = TRUE)
      
      # save gating set
      flowWorkspace::save_gs(gs, gsPath, overwrite = TRUE)
      gateObj$setImGatingSetFilepath(basename(gsPath), valueName = valueName)
      gateObj$saveState()
    }
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "applyAutospill",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # run
    run = function() {
      # get value name
      valueName <- self$funParams()$valueName
      
      # reset image information
      self$resetImageInfo(gatingSet = valueName)
      
      self$initLog()
      self$writeLog(">> Apply autospill")
      self$writeLog(">> https://autospill.vib.be/public/")
      
      # get object
      cciaObj <- self$cciaTaskObject()
      
      # create gating set
      if (cciaObj$getCciaClass() == "CciaImageSet") {
        # get compensation controls from set
        # adapted from
        # https://github.com/carlosproca/autospill/blob/master/inst/batch/calculate_compensation_website.r
        
        # set parameters
        asp <- autospill::get.autospill.param("website")
        # adjust to system cores
        # TODO probably don't need to set this
        asp$worker.process.n <- parallel::detectCores()

        # clear directory
        baseDir <- cciaObj$persistentObjectDirectoryFile(
          file.path("data", "autospill", valueName))
        unlink(baseDir, recursive = TRUE)

        # write marker definitions
        marker.file <- file.path(baseDir, "controls.csv")
        marker.controls <- file.path(baseDir, "controls")
        dir.create(marker.controls, recursive = TRUE, showWarnings = FALSE)

        # autospill will write everything into wd
        setwd(baseDir)

        # copy all single stain controls into one directory
        for (x in cciaObj$cciaObjects(uIDs = self$funParams()$uIDs)) {
          self$writeLog(x$oriFilepath())

          file.copy(x$oriFilepath(), marker.controls)
        }

        # create a csv with filenames for compensation
        # TODO this seems a bit odd just to pass filenames to read.marker
        # Do I have to provide more information?
        fcs.names <- as.data.frame(unname(
          sapply(cciaObj$cciaObjects(uIDs = self$funParams()$uIDs),
                 function(x) basename(x$oriFilepath()))
        ))
        colnames(fcs.names) <- c("file.name")
        fcs.names$filename <- fcs.names$file.name

        # extract parameters from compensation files
        # TODO this might have to be modified for custom compensation or Aurora
        fcs.names$dye <- paste0(
          stringr::str_extract(
            fcs.names$filename,"(?<=Controls_).*(?= Stained)"), "-A"
        )
        fcs.names$antigen <- "bead"
        fcs.names$wavelength <- ""

        # write marker file
        write.csv(fcs.names, marker.file)

        # calculate controls
        self$writeLog(">> Calculate controls")
        flow.control <- autospill::read.flow.control(marker.controls, marker.file, asp)

        # gate events before calculating spillover
        self$writeLog(">> Gate events")
        
        # TODO doesn't work
        # Sys.setenv(OBJC_DISABLE_INITIALIZE_FORK_SAFETY = "TRUE")
        Sys.setenv(OBJC_DISABLE_INITIALIZE_FORK_SAFETY = "YES")
        
        # too many errors related to OBJC_DISABLE_INITIALIZE_FORK_SAFETY
        # do the same here - maybe that makes a difference ...
        # flow.gate <- autospill::gate.flow.data(flow.control, asp)
        # flow.gate <- parallel::mclapply(flow.control$sample, function(samp)
        #   autospill:::do.gate(
        #     flow.control$expr.data.untr[flow.control$event.sample == samp,
        #                                  flow.control$scatter.parameter],
        #     flow.control$gate.parameter[[flow.control$marker.original[
        #       match(samp, flow.control$marker)]]],
        #     samp, flow.control, asp
        #   ),
        #   mc.cores = autospill:::get.worker.process(asp$worker.process.n)
        # )
        flow.gate <- lapply(flow.control$sample, function(samp) {
          self$writeLog(paste(">", samp))
          
          autospill:::do.gate(
            flow.control$expr.data.untr[flow.control$event.sample == samp,
                                        flow.control$scatter.parameter],
            flow.control$gate.parameter[[flow.control$marker.original[
              match(samp, flow.control$marker)]]],
            samp, flow.control, asp
          )
        })
        
        names(flow.gate) <- flow.control$sample
        
        # create a gating set with one gate from the convex hull
        # ie/ you can plot the compensation afterwards
        for (i in seq(length(flow.control$flow.set))) {
          private$createGatingSet(
            flow.control$flow.set[[i]], flow.gate[[i]],
            self$funParams()$uIDs[[i]], valueName)
        }
        
        # get initial spillover matrices from untransformed and transformed data
        marker.spillover.unco.untr <- autospill::get.marker.spillover(
          TRUE, flow.gate, flow.control, asp)
        marker.spillover.unco.tran <- autospill::get.marker.spillover(
          FALSE, flow.gate,flow.control, asp)

        # refine spillover matrix iteratively
        self$writeLog(">> Refine spillover matrix")
        refine.spillover.result <- autospill::refine.spillover(
          marker.spillover.unco.untr, marker.spillover.unco.tran, flow.gate, flow.control, asp)
      }
      
      # update image information
      cciaObj$setFlowAutospillPath(valueName, valueName = valueName)

      # save object
      cciaObj$saveState(includeChildren = FALSE)
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
