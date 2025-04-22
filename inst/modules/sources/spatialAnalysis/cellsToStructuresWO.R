CellsToStructuresWO <- R6::R6Class(
  "cellsToStructuresWO",
  inherit = SpatialAnalysis,
  
  private = list(
  ),
  
  public = list(
    # function name
    funName = function() {
      paste(
        super$funName(),
        "cellsToStructuresWO",
        sep = cecelia:::CCID_CLASS_SEP
      )
    },
    
    # calculate cell properties to structures
    calcCellsToStructures = function(tracksInfo, uns, coords, popDT, structuresPop) {
      # to which anisotropy does the track belong to?
      coordMappingsList <- list()
      
      for (x in unique(tracksInfo$uID)) {
        # can you calculate the anisotropy of local track/fibre?
        boxAnisotropy <- abs(uns[[structuresPop]][[x]]$ilee_eigval[1,,,1] - uns[[structuresPop]][[x]]$ilee_eigval[1,,,2])
        localLength <- uns[[structuresPop]][[x]]$ilee_box_total_length
        localAnisotropy <- boxAnisotropy / localLength[1,,]
        
        coordMapping <- (tracksInfo[uID == x] %>%
                           left_join(coords[[structuresPop]][[x]]$coords %>% tibble::rownames_to_column(),
                                     by = c("centroid_y_tile" = "y", "centroid_x_tile" = "x"))) %>%
          left_join(
            popDT[uID == x] %>%
              group_by(track_id) %>%
              arrange(label) %>%
              filter(row_number() == 1 | row_number() == n()) %>%
              summarise(y = diff(centroid_y), x = diff(centroid_x)), by = c("track_id"))
        
        matLocalAniso <- as.vector(localAnisotropy)[as.integer(coordMapping$rowname)]
        
        matA <- matrix(c(
          coords[[structuresPop]][[x]]$u[as.integer(coordMapping$rowname)],
          coords[[structuresPop]][[x]]$v[as.integer(coordMapping$rowname)]
        ), ncol = 2)
        
        matB <- matrix(c(
          coordMapping$x,
          coordMapping$y
        ), ncol = 2)
        
        matAngles <- c()
        matAniso <-  c()
        for (i in seq(nrow(matA))) {
          matAngles <- c(matAngles, matlib::angle(as.vector(matA[i, ]), as.vector(matB[i, ]))[1])
          # matAniso <- c(matAniso, norm(matA[i, ], type = "2"))
          matAniso <- c(matAniso, matLocalAniso[i])
        }
        
        coordMapping$angle <- matAngles
        coordMapping$aniso <- matAniso
        coordMapping <- as.data.table(coordMapping)
        
        # difference cannot be more than 90 DEG
        coordMapping[angle > 90, angle := 180 - angle]
        
        coordMappingsList[[x]] <- copy(coordMapping)
      }
      
      rbindlist(coordMappingsList)
    },
    
    # bootstrap cell properties to structures
    bootsrapCellsToStructures = function(tracksInfo, uns, coords, popDT,
                                         structuresPop, bootIterations = 100,
                                         aRad = 25) {
      runSim <- function(y) {
        x <- y$getUID()
        
        sizeX <- y$omeXMLPixels()$SizeX
        sizeY <- y$omeXMLPixels()$SizeY
        mappingsList <- list()
        
        # can you calculate the anisotropy of local track/fibre?
        boxAnisotropy <- abs(uns[[structuresPop]][[x]]$ilee_eigval[1,,,1] - uns[[structuresPop]][[x]]$ilee_eigval[1,,,2])
        localLength <- uns[[structuresPop]][[x]]$ilee_box_total_length
        localAnisotropy <- boxAnisotropy / localLength[1,,]
        
        for (bootID in seq(bootIterations)) {
          if (bootID %% 20 == 0)
            message(paste(">", x, bootID))
          
          # vary angle
          curPopsList <- list()
          for (i in popDT[uID == x]$track_id) {
            bootAngle <- runif(n=1, min=0, max=90)
            
            # vary angle
            curPopsList[[i]] <-
              rearrr::rotate_2d(
                popDT[uID == x & track_id == i] %>%
                  group_by(track_id) %>%
                  arrange(label) %>%
                  filter(row_number() == 1 | row_number() == n()),
                degrees = bootAngle,
                origin_fn = rearrr::centroid,
                x_col = "centroid_x",
                y_col = "centroid_y")
          }
          curPops <- rbindlist(curPopsList)
          
          coordMapping_rot <- (tracksInfo[uID == x] %>%
                                 left_join(coords[[structuresPop]][[x]]$coords %>% tibble::rownames_to_column(),
                                           by = c("centroid_y_tile" = "y", "centroid_x_tile" = "x"))) %>%
            left_join(
              # popDT[uID == x] %>%
              curPops %>%
                group_by(track_id) %>%
                # arrange(label) %>%
                # filter(row_number() == 1 | row_number() == n()) %>%
                summarise(
                  y = diff(centroid_y_rotated),
                  x = diff(centroid_x_rotated)),
              by = c("track_id"))
          
          # go through all tracks and vary position
          for (i in bootTracksInfo[uID == x]$track_id) {
            bootX <- runif(n=1, min=0, max=sizeX)
            bootY <- runif(n=1, min=0, max=sizeY)
            
            bootTracksInfo[uID == x & track_id == i, centroid_y_tile := round(bootY/aRad) * aRad]
            bootTracksInfo[uID == x & track_id == i, centroid_x_tile := round(bootX/aRad) * aRad]
          }
          
          coordMapping <- (bootTracksInfo[uID == x] %>%
                             left_join(coords[[structuresPop]][[x]]$coords %>% tibble::rownames_to_column(),
                                       by = c("centroid_y_tile" = "y", "centroid_x_tile" = "x")))
          
          matLocalAniso <- as.vector(localAnisotropy)[as.integer(coordMapping$rowname)]
          
          matA <- matrix(c(
            coords[[structuresPop]][[x]]$u[as.integer(coordMapping$rowname)],
            coords[[structuresPop]][[x]]$v[as.integer(coordMapping$rowname)]
          ), ncol = 2)
          
          matA_rot <- matrix(c(
            coords[[structuresPop]][[x]]$u[as.integer(coordMapping_rot$rowname)],
            coords[[structuresPop]][[x]]$v[as.integer(coordMapping_rot$rowname)]
          ), ncol = 2)
          
          # varied angles
          matB_rot <- matrix(c(
            coordMapping_rot$x,
            coordMapping_rot$y
          ), ncol = 2)
          
          matAngles <- c()
          matAniso <-  c()
          for (i in seq(nrow(matA))) {
            matAngles <- c(matAngles, matlib::angle(as.vector(matA_rot[i, ]), as.vector(matB_rot[i, ]))[1])
            # matAniso <- c(matAniso, norm(matA[i, ], type = "2"))
            matAniso <- c(matAniso, matLocalAniso[i])
          }
          
          coordMapping$angle <- matAngles
          coordMapping$aniso <- matAniso
          coordMapping <- as.data.table(coordMapping)
          
          # difference cannot be more than 90 DEG
          coordMapping[angle > 90, angle := 180 - angle]
          
          coordMapping$uID <- x
          coordMapping$bootID <- bootID
          
          mappingsList[[bootID]] <- coordMapping
        }
        
        mappingsList
      }
      
      # now .. can you also shuffle points and angles?
      coordMappingsList <- list()
      bootTracksInfo <- copy(tracksInfo)
      uIDs <- unique(bootTracksInfo$uID)
      
      coordMappingsList <- parallel::mclapply(
      # coordMappingsList <- lapply(
        self$cciaTaskObject()$cciaObjects(uIDs = uIDs),
        function(x) {
          message(sprintf("[run sim] >> %s", x$getUID()))
          runSim(x)
        }, mc.cores = parallel::detectCores() - 2
        # }
      )
      
      rbindlist(lapply(coordMappingsList, function(x) rbindlist(x)))
    },
    
    # run
    run = function() {
      # reset image information
      self$resetImageInfo()
      
      self$initLog()
      self$writeLog("Start workflow output")
      
      # get object
      # only works for experimental sets
      cciaObj <- self$cciaTaskObject()
      
      # define out directory
      outDir <- file.path(cciaObj$persistentObjectDirectory(), "out")
      
      # get uIDs
      uIDs <- NULL
      if ("uIDs" %in% names(self$funParams())) {
        uIDs <- self$funParams()$uIDs
      }
      
      exp.info <- as.data.table(cciaObj$summary(withSelf = FALSE, fields = c("Attr"), uIDs = uIDs))
      
      # get vector information to plot anisotropy
      self$writeLog("Get structure labels")
      
      # TODO these could be multiple. Only one for now
      unsLabels <- c(self$funParams()$structValueName)
      uns <- list()
      
      for (i in unsLabels) {
        uns[[i]] <- list()
        
        for (x in cciaObj$cciaObjects(uIDs = uIDs)) {
          labels <- x$labelProps(valueName = i)
          
          if (!is.null(labels)) {
            uns[[i]][[x$getUID()]] <- labels$values_uns()
            labels$close()
          }
        }
      }
      
      # merge summary
      exp.info <- exp.info[rbindlist(lapply(uns$nerves, function(x) x$ilee_summary), idcol = "uID"), on = c("uID")]
      
      # get branching
      self$writeLog("Get branching DT")
      
      branchDT <- cciaObj$popDT(popType = "branch", pops = unsLabels, uIDs = uIDs)
      
      pops <- self$funParams()$popsToAnalyse
      
      # get tracks to align with SHG
      popDT <- cciaObj$popDT(popType = "live", pops = pops, includeFiltered = TRUE, uIDs = uIDs)
      
      # get tracks info and check parameters
      tracksInfo <- cciaObj$tracksMeasures(pops = pops, uIDs = uIDs)
      
      # check where the centre of each track is
      tracksInfo[
        popDT[, lapply(.SD[, c("centroid_x", "centroid_y")], median), by = c("uID", "track_id")],
        on = c("uID", "track_id"),
        `:=` (centroid_x = centroid_x, centroid_y = centroid_y)]
      
      # get coords for quiver plots
      coords <- lapply(uns, function(x) {
        lapply(x, function(y) {
          coords <- data.frame(
            y = as.vector(y$ilee_coor_list[1,,,1]),
            x = as.vector(y$ilee_coor_list[1,,,2])
          )
          u <- as.vector(y$ilee_eigval[1,,,2]) * as.vector(y$ilee_eigvec[1,,,2,1])
          v <- as.vector(y$ilee_eigval[1,,,1]) * as.vector(y$ilee_eigvec[1,,,1,1])
          
          list(coords = coords, u = u, v = v)
        })
      })
      
      # get mappings of cells to structures
      aRad <- self$funParams()$aRad
      tracksInfo[, centroid_y_tile := round(centroid_y/aRad) * aRad]
      tracksInfo[, centroid_x_tile := round(centroid_x/aRad) * aRad]
      structuresPop <- unsLabels[[1]]
      
      self$writeLog("Calculate cells to structures")
      coordMappings <- self$calcCellsToStructures(
        tracksInfo, uns, coords, popDT, structuresPop)
      
      self$writeLog("Bootstrap cells to structures")
      # bootIterations <- self$funParams()$bootIterations
      bootIterations <- 10
      
      coordMappingsBoot <- self$bootsrapCellsToStructures(
        tracksInfo, uns, coords, popDT, structuresPop, bootIterations, aRad)
      
      # now bind observation and simulation
      self$writeLog("Combine observed and simulation")
      
      coordMappings$type <- "obs"
      coordMappings$bootID <- 0
      coordMappingsBoot$type <- "rand"
      
      coordMappingsTotal <- as.data.table(
        rbindlist(list(coordMappings, coordMappingsBoot), use.names = TRUE, fill = TRUE) %>%
          left_join(exp.info))
      
      # test for significance
      self$writeLog("Test for significance with t-test")
      sigValue <- self$funParams()$sigValue
      # TODO this should be able to take a list
      poolValue <- self$funParams()$poolValue # from exp.info
      pValues.angle <- list()
      pValues.aniso <- list()
      
      # now .. can you test for significance for each simulation?
      for (x in unique(coordMappingsTotal[[poolValue]])) {
        local_x <- local(x)
        self$writeLog(paste(">>", local_x))
        
        # count number of significant differences - angle
        xRes <- sapply(seq(bootIterations), function(i) {
          t.test(
            coordMappingsTotal[get(poolValue) == local_x & bootID == 0]$angle,
            coordMappingsTotal[get(poolValue) == local_x & bootID == i]$angle,
            na.action = na.omit
          )$p.value < sigValue
        })
        
        pValues.angle[[local_x]] <- (bootIterations - sum(xRes, na.rm = TRUE))/bootIterations
        
        # count number of significant differences - aniso
        xRes <- sapply(seq(bootIterations), function(i) {
          t.test(
            coordMappingsTotal[get(poolValue) == local_x & bootID == 0]$aniso,
            coordMappingsTotal[get(poolValue) == local_x & bootID == i]$aniso,
            na.action = na.omit
          )$p.value < sigValue
        })
        
        pValues.aniso[[local_x]] <- (bootIterations - sum(xRes, na.rm = TRUE))/bootIterations
      }
      
      p.file <- file.path(outDir, "p_values.csv") 
      cat(paste(c("measure", names(pValues.angle)), collapse = ","), file = p.file, sep = "\n")
      cat(paste(c("angle", pValues.angle), collapse = ","), file = p.file, sep = "\n", append = TRUE)
      cat(paste(c("aniso", pValues.aniso), collapse = ","), file = p.file, sep = "\n", append = TRUE)
      
      ## now plot out
      # ggplot doesn't work in forked process in R on MacOS, use Cairo
      # objc[9253]: +[UIFontDescriptor initialize] may have been in progress in another thread when fork() was called.
      self$writeLog("Generate plots")
      
      # plot properties
      plot.dpi <- 160
      plot.width <- 600
      plot.height <- 400
      
      p1 <- ggplot(coordMappingsTotal %>% left_join(exp.info) %>% dplyr::filter(aniso > 0), aes(type, angle)) +
        theme_classic() +
        # geom_boxplot(outlier.alpha = 1) +
        geom_violin(scale = "width") +
        # geom_jitter(width = 0.2) +
        # geom_jitter(position = position_jitterdodge(jitter.width = 0.20), alpha = 0.1) +
        # stat_summary(fun=mean, geom="point", size=8, shape=18, color="black")
        stat_summary(fun=mean, geom="point", size=8, shape=18, color="red") +
        # ylim(0, 100) +
        facet_grid(.~Region)
      
      Cairo::Cairo(plot.width, plot.height, file = file.path(outDir, "track_alignment_boot.png"), dpi = plot.dpi)
      print(p1)
      dev.off()
      
      data.table::fwrite(coordMappings %>% left_join(exp.info), file.path(outDir, "track_alignment.csv"))
      
      ## NEXT
      datToPlot <- coordMappingsTotal[type == "obs"] %>%
        left_join(coordMappingsTotal[type == "rand"] %>%
                    group_by(uID) %>%
                    summarise(mean.angle = mean(angle, na.rm = TRUE))) %>%
        mutate(angle.norm = angle/mean.angle) %>%
        dplyr::filter(aniso > 0)
      
      p1 <- ggplot(datToPlot %>% left_join(exp.info), aes(Region, angle.norm)) +
        theme_classic() +
        # geom_boxplot(outlier.alpha = 1) +
        geom_violin(scale = "width") +
        geom_jitter(width = 0.2) +
        # geom_jitter(position = position_jitterdodge(jitter.width = 0.20), alpha = 0.1) +
        stat_summary(fun=mean, geom="point", size=4, shape=18, color="red") +
        ylim(0, 8) + geom_hline(yintercept = 1)
      
      Cairo::Cairo(plot.width, plot.height, file = file.path(outDir, "track_alignment_boot_norm.png"), dpi = plot.dpi)
      print(p1)
      dev.off()
      
      data.table::fwrite(datToPlot %>% left_join(exp.info), file.path(outDir, "track_alignment_norm.csv"))
      
      ## NEXT
      p1 <- ggplot(coordMappingsTotal %>% left_join(exp.info) %>% dplyr::filter(aniso > 0), aes(type, aniso)) +
        theme_classic() +
        # geom_boxplot(outlier.alpha = 1) +
        geom_violin(scale = "width") +
        # geom_jitter(width = 0.2) +
        # geom_jitter(position = position_jitterdodge(jitter.width = 0.20), alpha = 0.1) +
        # stat_summary(fun=mean, geom="point", size=8, shape=18, color="black") +
        stat_summary(fun=mean, geom="point", size=8, shape=18, color="red") +
        ylim(0, 1) +
        facet_grid(.~Region)
      
      Cairo::Cairo(plot.width, plot.height, file = file.path(outDir, "track_aniso_boot.png"), dpi = plot.dpi)
      print(p1)
      dev.off()
      
      ## NEXT
      datToPlot <- coordMappingsTotal[type == "obs"] %>%
        left_join(coordMappingsTotal[type == "rand"] %>%
                    group_by(uID) %>%
                    summarise(mean.aniso = mean(aniso, na.rm = TRUE))) %>%
        mutate(aniso.norm = aniso/mean.aniso) %>%
        dplyr::filter(aniso > 0)
      
      p1 <- ggplot(datToPlot %>% left_join(exp.info), aes(Region, aniso.norm)) +
        theme_classic() +
        # geom_boxplot(outlier.alpha = 1) +
        geom_violin(scale = "width") +
        geom_jitter(width = 0.2) +
        # geom_jitter(position = position_jitterdodge(jitter.width = 0.20), alpha = 0.1) +
        stat_summary(fun=mean, geom="point", size=4, shape=18, color="red") +
        ylim(0, 10) + geom_hline(yintercept = 1)
      
      Cairo::Cairo(plot.width, plot.height, file = file.path(outDir, "track_aniso_boot_norm.png"), dpi = plot.dpi)
      print(p1)
      dev.off()
      
      data.table::fwrite(datToPlot %>% left_join(exp.info), file.path(outDir, "track_aniso_norm.csv"))
      
      # plot vessel measurements for each type
      vesselMeasures <- c(
        "occupancy", "linear_density (PU/PU^2)", "skewness", "cv",
        "Diameter_tdt (PU)", "Diameter_sdt (PU)", "sev_act (/PU of filament)",
        "branching_act(/PU of filament)", "anisotropy" 
      )
      
      datToPlot <- exp.info %>%
        pivot_longer(cols = vesselMeasures,
                     names_to = "measure", values_to = "value")
      
      p1 <- ggplot(datToPlot, aes(Region, value)) +
        theme_classic() +
        geom_boxplot(outlier.alpha = 0) +
        geom_jitter(width = 0.2) +
        # geom_jitter(position = position_jitterdodge(jitter.width = 0.10)) +
        facet_wrap(.~measure, scales = "free", nrow = 2) +
        theme(legend.position = "bottom") +
        scale_color_brewer(palette = "Set1") +
        expand_limits(y = 0)
      # scale_color_manual(values = c("#327EBA", "#AA1F5E"))
      
      Cairo::Cairo(1800, 600, file = file.path(outDir, "structures.png"), dpi = plot.dpi)
      print(p1)
      dev.off()
      
      data.table::fwrite(exp.info, file.path(outDir, "structures.csv"))
      
      ## NEXT
      self$writeLog("Generate flows")
      for (x in cciaObj$cciaObjects(uIDs = uIDs)) {
        p1 <- ggplot(popDT[uID == x$getUID()]) + 
          # scale_color_brewer(palette = "Set1") +
          theme_classic() +
          plotThemeDark() +
          coord_fixed() +
          xlim(0, x$omeXMLPixels()$SizeX) +
          ylim(x$omeXMLPixels()$SizeY, 0) +
          theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none"
          ) 
        
        yCoords <- coords[[structuresPop]][[x$getUID()]]
        
        if (all(c(!is.null(yCoords)))) {
          quiver.data <- cbind(yCoords$coords, yCoords$u, yCoords$v)
          colnames(quiver.data) <- c("x", "y", "u", "v")
          
          p1 <- p1 + 
            # geom_path(aes(centroid_x, centroid_y, group = track_id, colour = as.factor(track_id)),
            # linewidth = 2, alpha = 1) +
            geom_segment(
              data = branchDT[uID == x$getUID() & pop == structuresPop],
              aes(x = `coord-src-1`, y = `coord-src-0`,
                  xend = `coord-dst-1`, yend = `coord-dst-0`),
              # color = "#ebd441", linewidth = 1, alpha = 1) +
              color = "#AA1F5E", linewidth = 1, alpha = 1) +
            # quiver doesn't work with Cairo
            # ggquiver::geom_quiver(
            #   data = yCoords$coords, aes(x = x, y = y, u = -yCoords$u, v = yCoords$v), color = "#4682b4") 
            # https://jtr13.github.io/cc22tt/tutorial-for-vector-fields-in-r.html
            geom_segment(data = quiver.data, aes(
              y = x, x = y,
              yend = x - (v * 0.1), 
              xend = y + (u * 0.1)), 
              arrow = arrow(length = unit(0.2, "cm")),
              size = 0.5, color = "#4682b4") +
            geom_path(aes(centroid_x, centroid_y, group = track_id),
                      # color = "#AA1F5E", linewidth = 1, alpha = 1) +
                      color = "white", linewidth = 1, alpha = 1) +
            coord_fixed()
        }
        
        y <- exp.info[uID == x$getUID()]
        
        Cairo::Cairo(1200, 1200, file = file.path(outDir, paste0("flows-", y[[poolValue]], "-", x$getUID(), ".png")), dpi = plot.dpi)
        print(p1)
        dev.off()
      }
      
      # DONE
      self$writeLog("Done")
      self$exitLog()
      
      # update image information
      self$updateImageInfo()
      
      TRUE
    }
  )
)
