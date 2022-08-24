ApplyAutospill <- R6::R6Class(
  "ApplyAutospill",
  inherit = GatePopulations,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "applyAutospill",
        sep = CCID_CLASS_SEP
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
        
        # objc[30144]: +[NSPlaceholderString initialize] may have been in progress in another thread when fork() was called.
        # https://github.com/darkskyapp/forecast-ruby/issues/13#issuecomment-581025826
        # https://github.com/darkskyapp/forecast-ruby/issues/13#issuecomment-593375790
        Sys.setenv(
          OBJC_DISABLE_INITIALIZE_FORK_SAFETY = "TRUE",
          DISABLE_SPRING = "true")
        
        # set parameters
        asp <- autospill::get.autospill.param("website")
        # adjust to system cores
        # asp$worker.process.n <- parallel::detectCores() - 1
        asp$worker.process.n <- 3
        
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
        flow.gate <- autospill::gate.flow.data(flow.control, asp)
        
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
      cciaObj$setImGatingSetFilepath(valueName, valueName = valueName)

      # save object
      cciaObj$saveState()
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
    }
  )
)
