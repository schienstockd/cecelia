library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

devtools::load_all("../../")
cciaUse("~/cecelia/dev")
# cciaUse("~/cecelia/dev", initConda = FALSE)

# source all files from subdirectories
# TODO is there a better way of doing this?
appSources <- c(
  file.path("..", "inst", "app", "constantsCore.R"),
  list.files(file.path("..", "inst", "app", "lib"), pattern = ".R$", recursive = TRUE, full.names = TRUE),
  list.files(file.path("..", "inst", "app", "helpers"), pattern = ".R$", recursive = TRUE, full.names = TRUE)
)

for (x in appSources) {
  source(x)
}

# confidence level
confidencePercentage = 95
confidenceLevel = confidencePercentage/100

# # SPLEEN MILAS 2D
# pID <- "pEdOoZ"
# versionID <- 2
# uID <- "uxOpZJ"

# # BEHAVIOUR H2KB 3D+T
# pID <- "8BR53W"
# versionID <- 1
# uID <- "YJRfCW"

# # XCR1-venus vibratome 3D
# pID <- "Lq0joh"
# versionID <- 11
# uID <- "VbS3EJ"

# Arm Spleen
pID <- "pEdOoZ"
versionID <- 2
uID <- "ktUu0n"

id <- "plotCharts"
ns <- NS(id)

ui <- fluidPage(
  fluidRow(
    shinydashboard::box(
      id = ns("plotData"),
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Data selection",
      status = "primary",
      width = 12,
      uiOutput(ns("plotData"))
    )
  ),
  fluidRow(
    shinydashboard::box(
      id = ns("plots"),
      solidHeader = TRUE,
      collapsible = TRUE, 
      title = "Plot output",
      status = "primary",
      width = 12,
      column(3, uiOutput(ns("plotParams"))),
      column(9, uiOutput(ns("plotOutput")))
    )
  )
)

server <- function(input, output, session) {
  moduleServer(
    id,
    function(input, output, session) {
      ### Functions
      
      ### Reactive values
      popDT <- reactiveVal()
      multiplePops <- reactiveVal(FALSE)
      numUIDs <- reactiveVal(0)
      pointsDT <- reactiveVal()
      summaryDT <- reactiveVal()
      expInfoUpdated <- reactiveVal()
      
      ### Reactive-like values
      
      ### Reactives - RxCalc
      ## Event specific
      # update image automatically when populations are gated
      updateImage <- eventReactive(c(
        cciaObj()
      ), {
        req(cciaObj())
        
        # update image
        runif(1)
      })
      
      ## Generic
      # DEBUG
      selectedUIDs <- reactive({
        req(expInfo())
        
        expInfo()$uID
        
        # MILAS SPLEEN
        # expInfo()[dpi == "1-5" & Genotype != "zDC"]$uID
        
        # Arm Spleen
        c("TURHVv")
        
        # # behaviour DTx
        # uIDs[!uIDs %in% c(
        #   "5N8Iip", "OWJrYz", "PxwhNn",
        #   "CzR7ZQ", "zqrpfq",
        #   "NbaQvC", "ypUN8d", "oPmJg0",
        #   "o0auGO", "TxTL0a"
        # )]
      })
      
      # pop type
      popType <- reactive({
        input$resultParamsPopType
      })
      
      # experimental info
      expInfo <- reactive({
        req(cciaSet())
        
        as.data.table(cciaSet()$summary(withSelf = FALSE, fields = c("Attr")))
      })
      
      # get all populations
      popsAll <- reactive({
        req(cciaObj())
        
        cciaObj()$popPathsAll(includeFiltered = TRUE, flattenPops = TRUE)
      })
      
      # all populations
      resultParamsPopsInt <- reactive({
        input$resultParamsPopsInt
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # populations to get
      resultParamsPops <- reactive({
        input$resultParamsPops
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # populations to show
      resultParamsPopsShow <- reactive({
        input$resultParamsPopsShow
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # categories from summary
      resultParamsCats <- reactive({
        req(popDT())
        
        paramCats <- c()
        
        if (!is.null(resultSummaryAxisY())) {
          if (.cciaStatsTypeIsCategorical(resultSummaryAxisY())) {
            paramCats <- popDT()[, unique(get(resultSummaryAxisY()))]
          }
        }
        
        paramCats
      })
      
      # categories to show
      resultParamsCatsShow <- reactive({
        input$resultParamsCatsShow
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # properties to show
      resultParamsCols <- reactive({
        input$resultParamsCols
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # summary properties to show
      resultSummaryAxisX <- reactive({
        input$resultSummaryAxisX
      })
      
      resultSummaryAxisXCompiled <- reactive({
        req(resultSummaryAxisX())
        req(resultSummaryInteraction())
        
        if (resultSummaryInteraction() != "NONE")
          paste0(resultSummaryAxisX(), ".", resultSummaryInteraction())
        else
          resultSummaryAxisX()
      })
      
      resultSummaryAxisXList <- reactive({
        req(resultSummaryAxisXCompiled())
        
        strsplit(resultSummaryAxisXCompiled(), "\\.")[[1]]
      })
      
      resultSummaryAxisY <- reactive({
        input$resultSummaryAxisY
      })
      
      resultSummaryAxisYCat <- reactive({
        req(resultSummaryAxisY())
        
        if (.cciaStatsTypeIsCategorical(resultSummaryAxisY())) {
          paste0(resultSummaryAxisY(), ".cat")
        } else {
          NULL
        }
      })
      
      resultSummaryAxisYCompiled <- reactive({
        req(resultSummaryAxisY())
        
        list(
          mean = paste0(resultSummaryAxisY(), ".mean"),
          median = paste0(resultSummaryAxisY(), ".median"),
          min = paste0(resultSummaryAxisY(), ".min"),
          max = paste0(resultSummaryAxisY(), ".max"),
          meanCiLo = paste0(resultSummaryAxisY(), ".meanCiLo"),
          meanCiHi = paste0(resultSummaryAxisY(), ".meanCiHi"),
          medianCiLo = paste0(resultSummaryAxisY(), ".medianCiLo"),
          medianCiHi = paste0(resultSummaryAxisY(), ".medianCiHi")
        )
      })
      
      resultSummaryInteraction <- reactive({
        input$resultSummaryInteraction
      })
      
      # plot properties
      plotWidth <- reactive({
        input$plotWidth
      })
      
      plotHeight <- reactive({
        input$plotHeight
      })
      
      # points data that is shown
      pointsData <- eventReactive(c(
        input$plotOutputTabs,
        popDT(),
        pointsDT(),
        expInfoUpdated(),
        resultParamsPopsShow(),
        resultParamsCatsShow()
      ), {
        req(input$plotOutputTabs)
        req(popDT())
        req(pointsDT())
        req(expInfo())
        
        # TODO version without copy
        DT <- NULL
        if (input$plotOutputTabs == "combined")
          DT <- pointsDT()
        else
          DT <- popDT()[expInfo(), on = .(uID)]
        
        if (!is.null(resultParamsPopsShow()) && length(resultParamsPopsShow()) > 0)
          DT <- DT %>% dplyr::filter(pop %in% resultParamsPopsShow())
        if (!is.null(resultParamsCatsShow()) && length(resultParamsCatsShow()) > 0)
          DT <- DT %>% dplyr::filter(get(resultSummaryAxisYCat()) %in% resultParamsCatsShow())
        
        DT
      })
      
      # points data that is shown
      summaryData <- reactive({
        req(input$plotOutputTabs)
        req(pointsDT())
        req(summaryDT())
        
        # TODO version without copy
        DT <- NULL
        if (input$plotOutputTabs == "combined")
          DT <- summaryDT()
        else
          DT <- pointsDT()
        
        if (!is.null(resultParamsPopsShow()) && length(resultParamsPopsShow()) > 0)
          DT <- DT %>% dplyr::filter(pop %in% resultParamsPopsShow())
        if (!is.null(resultParamsCatsShow()) && length(resultParamsCatsShow()) > 0) {
          DT <- DT %>% dplyr::filter(get(resultSummaryAxisYCat()) %in% resultParamsCatsShow())
        }
        
        DT
      })
      
      # plot data that is shown
      plotData <- reactive({
        req(input$plotOutputTabs)
        req(summaryPlotData())
        req(indvPlotData())
        
        if (input$plotOutputTabs == "combined")
          summaryPlotData()
        else
          indvPlotData()
      })
      
      # individual data for plot
      indvPlotData <- reactive({
        req(summaryPlotData())
        
        summaryPlotData() +
          facet_wrap(.~uID, ncol = 6) +
          theme(
            strip.background = element_rect(fill = NA, color = "black"),
            strip.text.x = element_text(color = "black")
          )
      })
      
      # summary data for plot
      summaryPlotData <- reactive({
        req(pointsData())
        req(summaryData())
        
        widthColumn <- 0.7
        
        # Change linecolor in case of dark mode
        if (input$darkTheme) {
          lineColor <- "grey80"
        } else if (input$darkTheme == FALSE) {
          lineColor <- "black"
        } 
        
        # alternative colour paletes
        newColors <- NULL
        
        if (!is.null(input$adjustColors)) {
          if (input$adjustColors == 2) {
            newColors <- .plotColourTolBright
          } else if (input$adjustColors == 3) {
            newColors <- .plotColourTolMuted
          } else if (input$adjustColors == 4) {
            newColors <- .plotColourTolLight
          } else if (input$adjustColors == 6) {
            .plotColourOkabeIto[8] <- lineColor
            newColors <- .plotColourOkabeIto
          } else if (input$adjustColors == 5) {
            newColors <- gsub("\\s","", strsplit(input$userColorList,",")[[1]])
          }
        }
        
        # define if color is used for the data
        if (input$colorData == FALSE) {
          colorData <- NULL
        } else if (input$colorData == TRUE) {
          colorData <- "Condition"
        }
        
        if (input$colorData == TRUE || input$colorStats == TRUE) {    
          # determine the number of colors that are necessary
          maxColors <- nlevels(resultSummaryAxisXCompiled())
          
          # if unsufficient colors available, repeat
          if(length(newColors) < maxColors) {
            newColors <- rep(newColors,
                             times = (round(maxColors/length(newColors))) + 1)
          }
        }
        
        if (input$colorStats == FALSE) {
          colorStats <- NULL
        } else if (input$color_stats == TRUE) {
          colorStats <- "Condition"
        }  
        
        # define minimal n - only plot box/violinplots for minN > 9
        minN <- min(summaryData()$n)
        
        if (input$jitterType == "none")
          widthColumn <- widthColumn/2
        
        # generate plot layers
        p1 <- ggplot(data = summaryData(), aes(x = get(resultSummaryAxisXCompiled())))
        
        # set the order of the x-axis
        # TODO how to do this?
        # p1 <- p1 + scale_x_discrete(limits = custom_order)
        
        xlabTitle <- resultSummaryAxisXCompiled()
        ylabTitle <- "y"
        
        if (input$summaryInput %in% c("median", "box", "violin"))
          ylabTitle <- resultSummaryAxisYCompiled()$median
        else if (input$summaryInput == "mean")
          ylabTitle <- resultSummaryAxisYCompiled()$mean
        
        # add bar/box as visual aid
        if (input$addBar == TRUE) {
          if (input$summaryInput == "median") {
            if (multiplePops()) {
              p1 <- p1 + stat_summary(
                data = pointsData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisY()),
                    color = pop),
                fun = median, fun.min = min, fun.max = max,
                geom = "crossbar", width = widthColumn,
                fill = NA, alpha = input$alphaInputSum/4,
                position = position_dodge(width = widthColumn))
            } else {
              p1 <- p1 + stat_summary(
                data = pointsData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisY())),
                fun = median, fun.min = min, fun.max = max,
                geom = "crossbar", width = widthColumn, color = NA, fill = "grey",
                alpha = input$alphaInputSum/4)
            }
          } else if (input$summaryInput == "mean") {
            if (multiplePops()) {
              p1 <- p1 + stat_summary(
                data = pointsData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisY()),
                    color = pop),
                fun.y = mean, fun.ymin = min, fun.ymax = max,
                geom = "crossbar", width = widthColumn,
                fill = NA, alpha = input$alphaInputSum/4,
                position = position_dodge(width = widthColumn))
            } else {
              p1 <- p1 + stat_summary(
                data = pointsData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisY())),
                fun.y = mean, fun.ymin = min, fun.ymax = max,
                geom = "crossbar", width = widthColumn, color = NA, fill = "grey",
                alpha = input$alphaInputSum/4)
            }
          }
        }
        
        # plot selected data summary (bottom layer)
        # if (input$summaryInput == "box" && minN > 9) {
        if (input$summaryInput == "box") {
          p1 <- p1 + geom_boxplot(
            data = pointsData(),
            aes(x = get(resultSummaryAxisXCompiled()),
                y = get(resultSummaryAxisY()),
                color = pop),
            notch = input$addCI, outlier.color = NA,
            # width = widthColumn, size = 0.5, alpha = input$alphaInputSum)
            size = 0.5, alpha = input$alphaInputSum,)
        # } else if (input$summaryInput == "violin" && minN > 9) {
        } else if (input$summaryInput == "violin") {
          p1 <- p1 + geom_violin(
            data = pointsData(),
            aes(x = get(resultSummaryAxisXCompiled()),
                y = get(resultSummaryAxisY()),
                color = pop),
            scale = "width", width = widthColumn,
            size = 1, alpha = input$alphaInputSum) 
        }
        
        # plot individual measurements (middle layer)
        if (input$jitterType == "quasirandom") {
          p1 <- p1 + geom_quasirandom(
            data = pointsData(),
            aes(x = get(resultSummaryAxisXCompiled()),
                y = get(resultSummaryAxisY()),
                fill = pop, group = pop),
            shape = 21, varwidth = TRUE, dodge.width = widthColumn, cex = 3.5, alpha = input$alphaInput)
        # } else if (input$jitterType == "sina") {
        #   p1 <- p1 + geom_sina(
        #     data = pointsData(),
        #     aes(x = get(resultSummaryAxisXCompiled()),
        #         y = get(resultSummaryAxisY()),
        #         color = pop, group = pop),
        #     method = "density", maxwidth = .8, cex = 3, alpha = input$alphaInput)
        } else if (input$jitterType == "random") {
          if (multiplePops()) {
            p1 <- p1 + geom_jitter(
              data = pointsData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  y = get(resultSummaryAxisY()),
                  fill = pop, group = pop),
              shape = 21, cex = 3.5, alpha = input$alphaInput,
              position = position_jitterdodge(jitter.width = widthColumn/2))
          } else {
            p1 <- p1 + geom_jitter(
              data = pointsData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  y = get(resultSummaryAxisY()),
                  fill = pop, group = pop),
              shape = 21, width = widthColumn, height = 0.0, cex = 3.5, alpha = input$alphaInput)
          }
        # } else if (input$jitterType == "stripes") {
        #   p1 <- p1 + geom_segment(
        #     data = summaryData(),
        #     aes(x = match(Condition, levels(Condition))-((widthColumn/2)-0.1), xend=match(Condition, levels(Condition))+((widthColumn/2)-0.1), y=Value, yend=Value, color =kleur), size=1, alpha=input$alphaInput)
        } else if (input$jitterType == "none") {
          if (multiplePops()) {
            p1 <- p1 + geom_jitter(
              data = pointsData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  y = get(resultSummaryAxisY()),
                  fill = pop, group = pop),
              shape = 21, cex = 3.5, alpha = input$alphaInput,
              position = position_jitterdodge(jitter.width = 0.0))
          } else {
            p1 <- p1 + geom_jitter(
              data = pointsData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  y = get(resultSummaryAxisY()),
                  fill = pop, group = pop),
              shape = 21, width = 0, height = 0.0, cex = 3.5, alpha = input$alphaInput)
          }
        }
        
        # plot selected data summary (top layer)
        # if (input$summaryInput == "median"  && input$addCI == TRUE && minN > 9) {
        if (input$summaryInput == "median" && input$addCI == TRUE) {
          if (!input$errorBars) {
            if (multiplePops()) {
              p1 <- p1 + geom_point(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisYCompiled()$median),
                    color = pop, group = pop),
                shape = 21, fill = NA, size = 8, alpha = input$alphaInputSum,
                position = position_dodge(width = widthColumn)) +
                geom_linerange(
                  data = summaryData(),
                  aes(
                    x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$medianCiLo),
                    ymax = get(resultSummaryAxisYCompiled()$medianCiHi),
                    color = pop, group = pop),
                  size = 3, alpha = input$alphaInputSum,
                  position = position_dodge(width = widthColumn))
            } else {
              p1 <- p1 + geom_point(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisYCompiled()$median),
                    color = pop, group = pop),
                shape = 21, fill = NA, size = 8, alpha = input$alphaInputSum) +
                geom_linerange(
                  data = summaryData(),
                  aes(
                    x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$medianCiLo),
                    ymax = get(resultSummaryAxisYCompiled()$medianCiHi),
                    color = pop, group = pop),
                  size = 3, alpha = input$alphaInputSum)
            }
          } else {
            if (multiplePops()) {
              p1 <- p1 + geom_errorbar(
                data = summaryData(),
                aes(
                  x = get(resultSummaryAxisXCompiled()),
                  ymin = get(resultSummaryAxisYCompiled()$median),
                  ymax = get(resultSummaryAxisYCompiled()$median),
                  color = pop, group = pop),
                size = 1, alpha = input$alphaInputSum, width = widthColumn/2,
                position = position_dodge(width = widthColumn)) +
                geom_errorbar(
                  data = summaryData(),
                  aes(x = get(resultSummaryAxisXCompiled()),
                      ymin = get(resultSummaryAxisYCompiled()$medianCiLo),
                      ymax = get(resultSummaryAxisYCompiled()$medianCiHi),
                      color = pop, group = pop),
                  size = 1, alpha = input$alphaInputSum, width = widthColumn/4,
                  position = position_dodge(width = widthColumn))
            } else {
              p1 <- p1 + geom_errorbar(
                data = summaryData(),
                aes(
                  x = get(resultSummaryAxisXCompiled()),
                  ymin = get(resultSummaryAxisYCompiled()$median),
                  ymax = get(resultSummaryAxisYCompiled()$median),
                  color = pop, group = pop),
                width = widthColumn, size = 1, alpha = input$alphaInputSum) +
                geom_errorbar(
                  data = summaryData(),
                  aes(x = get(resultSummaryAxisXCompiled()),
                      ymin = get(resultSummaryAxisYCompiled()$medianCiLo),
                      ymax = get(resultSummaryAxisYCompiled()$medianCiHi),
                      color = pop, group = pop),
                  width = widthColumn/2, size = 1, alpha = input$alphaInputSum)
            }
          }
        } else if (input$summaryInput == "median" && minN < 10) {
          if (multiplePops()) {
            p1 <- p1 + geom_errorbar(
              data = summaryData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  ymin = get(resultSummaryAxisYCompiled()$median),
                  ymax = get(resultSummaryAxisYCompiled()$median),
                  color = pop, group = pop),
              size = 1, alpha = input$alphaInputSum,
              position = position_dodge(width = widthColumn))
          } else {
            p1 <- p1 + geom_errorbar(
              data = summaryData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  ymin = get(resultSummaryAxisYCompiled()$median),
                  ymax = get(resultSummaryAxisYCompiled()$median),
                  color = pop, group = pop),
              width = widthColumn, size = 1, alpha = input$alphaInputSum)
          }
        # } else if (input$summaryInput == "mean" && input$addCI == TRUE && minN > 9) {
        } else if (input$summaryInput == "mean" && input$addCI == TRUE) {
          if (!input$errorBars) {
            if (multiplePops()) {
              p1 <- p1 + geom_linerange(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$meanCiLo),
                    ymax = get(resultSummaryAxisYCompiled()$meanCiHi),
                    color = pop, group = pop),
                size = 3, alpha = input$alphaInputSum,
                position = position_dodge(width = widthColumn)) +
                geom_point(
                  data = summaryData(), aes(
                    x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisYCompiled()$mean),
                    color = pop, group = pop),
                  shape = 21, fill = NA, size = 8, alpha = input$alphaInputSum,
                  position = position_dodge(width = widthColumn))
            } else {
              p1 <- p1 + geom_linerange(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$meanCiLo),
                    ymax = get(resultSummaryAxisYCompiled()$meanCiHi),
                    color = pop, group = pop),
                size = 3, alpha = input$alphaInputSum) +
                geom_point(
                  data = summaryData(), aes(
                    x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisYCompiled()$mean),
                    color = pop, group = pop),
                  shape = 21, fill = NA, size = 8, alpha = input$alphaInputSum)
            }
          } else {
            # p1 <- p1 + geom_errorbar(
            if (multiplePops()) {
              p1 <- p1 + geom_errorbar(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$mean),
                    ymax = get(resultSummaryAxisYCompiled()$mean),
                    color = pop, group = pop),
                size = 1, alpha = input$alphaInputSum, width = widthColumn/2,
                position = position_dodge(width = widthColumn)) +
                geom_errorbar(
                  data = summaryData(),
                  aes(x = get(resultSummaryAxisXCompiled()),
                      ymin = get(resultSummaryAxisYCompiled()$meanCiLo),
                      ymax = get(resultSummaryAxisYCompiled()$meanCiHi),
                      color = pop, group = pop),
                  size = 1, alpha = input$alphaInputSum, width = widthColumn/4,
                  position = position_dodge(width = widthColumn))
            } else {
              p1 <- p1 + geom_errorbar(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$mean),
                    ymax = get(resultSummaryAxisYCompiled()$mean),
                    color = pop, group = pop),
                width = widthColumn, size = 1, alpha = input$alphaInputSum) +
                geom_errorbar(
                  data = summaryData(),
                  aes(x = get(resultSummaryAxisXCompiled()),
                      ymin = get(resultSummaryAxisYCompiled()$meanCiLo),
                      ymax = get(resultSummaryAxisYCompiled()$meanCiHi),
                      color = pop, group = pop),
                  width = widthColumn/2, size = 1, alpha = input$alphaInputSum)
            }
          }
        } else if (input$summaryInput == "mean" && minN < 10) {
          if (multiplePops()) {
            p1 <- p1 + geom_errorbar(
              data = summaryData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  y = get(resultSummaryAxisYCompiled()$mean),
                  ymin = get(resultSummaryAxisYCompiled()$mean),
                  ymax = get(resultSummaryAxisYCompiled()$mean),
                  color = pop, group = pop),
              size = 1, alpha = input$alphaInputSum, width = widthColumn/2,
              position = position_dodge(width = widthColumn))
          } else {
            p1 <- p1 + geom_errorbar(
              data = summaryData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  y = get(resultSummaryAxisYCompiled()$mean),
                  ymin = get(resultSummaryAxisYCompiled()$mean),
                  ymax = get(resultSummaryAxisYCompiled()$mean),
                  color = pop, group = pop),
              width = widthColumn, size = 1, alpha = input$alphaInputSum)
          }
        } else if (input$summaryInput == "median"  && minN > 9 && input$addCI == FALSE) {
          if (multiplePops()) {
            p1 <- p1 + geom_errorbar(
              data = summaryData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  ymin = get(resultSummaryAxisYCompiled()$median),
                  ymax = get(resultSummaryAxisYCompiled()$median),
                  color = pop, group = pop),
              size = 2, alpha = input$alphaInputSum, width = widthColumn/2,
              position = position_dodge(width = widthColumn))
          } else {
            p1 <- p1 + geom_errorbar(
              data = summaryData(),
              aes(x = get(resultSummaryAxisXCompiled()),
                  ymin = get(resultSummaryAxisYCompiled()$median),
                  ymax = get(resultSummaryAxisYCompiled()$median),
                  color = pop, group = pop),
              width = widthColumn, size = 2, alpha = input$alphaInputSum)
          }
        } else if (input$summaryInput == "mean" && minN > 9 && input$addCI == FALSE) {
          p1 <- p1 + geom_errorbar(
            data = summaryData(),
            aes(x = get(resultSummaryAxisXCompiled()),
                ymin = get(resultSummaryAxisYCompiled()$mean),
                ymax = get(resultSummaryAxisYCompiled()$mean),
                color = pop, group = pop),
            width = widthColumn, size = 2, alpha = input$alphaInputSum)
        } else if (input$summaryInput == "violin" && minN > 9 && input$addCI == FALSE) {
          p1 <- p1 + geom_point(
            data = summaryData(),
            aes(x = get(resultSummaryAxisXCompiled()),
                y = get(resultSummaryAxisYCompiled()$median)),
            color = lineColor, shape = 21, fill = NA, size = 8, alpha = input$alphaInputSum)
        # } else if (input$summaryInput == "violin" && minN > 3 && input$addCI == TRUE) {
        } else if (input$summaryInput == "violin" && input$addCI == TRUE) {
          if (!input$errorBars) {
            if (multiplePops()) {
              p1 <- p1 + geom_point(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisYCompiled()$median),
                    group = pop),
                color = lineColor, shape = 21, fill = NA,
                size = 8, alpha = input$alphaInputSum, stroke = 2,
                position = position_dodge(width = widthColumn))
              p1 <- p1 + geom_linerange(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$medianCiLo),
                    ymax = get(resultSummaryAxisYCompiled()$medianCiHi),
                    color = pop, group = pop),
                color = lineColor, size = 2, alpha = input$alphaInputSum,
                position = position_dodge(width = widthColumn))
            } else {
              p1 <- p1 + geom_point(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    y = get(resultSummaryAxisYCompiled()$median)),
                color = lineColor, shape = 21, fill = NA,
                size = 8, alpha = input$alphaInputSum)
              p1 <- p1 + geom_linerange(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$medianCiLo),
                    ymax = get(resultSummaryAxisYCompiled()$medianCiHi),
                    color = pop, group = pop),
                color = lineColor, size = 3, alpha = input$alphaInputSum)
            }
          } else {
            if (multiplePops()) {
              p1 <- p1 + geom_errorbar(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$median),
                    ymax = get(resultSummaryAxisYCompiled()$median),
                    color = pop, group = pop),
                color = lineColor, width = widthColumn*0.95,
                size = 1, alpha = input$alphaInputSum,
                position = position_dodge(width = widthColumn)) +
                geom_errorbar(
                  data = summaryData(),
                  aes(x = get(resultSummaryAxisXCompiled()),
                      ymin = get(resultSummaryAxisYCompiled()$medianCiLo),
                      ymax = get(resultSummaryAxisYCompiled()$medianCiHi),
                      color = pop, group = pop),
                  color = lineColor, width = widthColumn/2,
                  size = 1, alpha = input$alphaInputSum,
                  position = position_dodge(width = widthColumn))
            } else {
              p1 <- p1 + geom_errorbar(
                data = summaryData(),
                aes(x = get(resultSummaryAxisXCompiled()),
                    ymin = get(resultSummaryAxisYCompiled()$median),
                    ymax = get(resultSummaryAxisYCompiled()$median),
                    color = pop, group = pop),
                color = lineColor, width = widthColumn*0.95,
                size = 1, alpha = input$alphaInputSum) +
                geom_errorbar(
                  data = summaryData(),
                  aes(x = get(resultSummaryAxisXCompiled()),
                      ymin = get(resultSummaryAxisYCompiled()$medianCiLo),
                      ymax = get(resultSummaryAxisYCompiled()$medianCiHi),
                      color = pop, group = pop),
                  color = lineColor, width = widthColumn/2,
                  size = 1, alpha = input$alphaInputSum)
            }
          }
        }
        
        # format layout
        p1 <- p1 + theme_light(base_size = 16)
        if (input$darkTheme) {p1 <- p1 + theme_darker(base_size = 16)}
        
        # if log-scale checked specified
        if (input$scaleLog10)
          p1 <- p1 + scale_y_log10() 
        
        # adjust scale if range (min, max) is specified
        if (input$range != "" &&  input$changeScale == TRUE) {
          rng <- as.numeric(strsplit(input$range,",")[[1]])
          
          # if min > max invert the axis
          if (rng[1] > rng[2]) {p1 <- p1 + scale_y_reverse()}
          
          # autoscale if rangeis NOT specified
        } else if (input$range == "" || input$changeScale == FALSE) {
          rng <- c(NULL, NULL)
        }
        
        p1 <- p1 + coord_cartesian(ylim=c(rng[1],rng[2]))
        
        # If selected, rotate plot 90 degrees C
        if (input$rotatePlot == TRUE) {
          p1 <- p1 + coord_flip(ylim = c(rng[1], rng[2]))
        }
        
        # if title specified
        if (input$addTitle)
          p1 <- p1 + ggtitle(input$title)
        
        # if tidy data, use the labels from selected columns
        if (!is.null(input$labelAxes)) {
          if (!is.null(input$tidyInput)) {
            if (!input$labelAxes && input$tidyInput == TRUE) {
              xlabTitle <- paste(input$labX)
              ylabTitle <- paste(input$labY)
            }
          } else if (input$labelAxes) {
            xlabTitle <- input$labX
            ylabTitle <- input$labY
          }
        }
        
        # if font size is adjusted
        if (input$adjFontSize == TRUE) {
          p1 <- p1 + theme(axis.text = element_text(size = input$adjFontSizeAxLabels))
          p1 <- p1 + theme(axis.title = element_text(size = input$adjFontSizeAxTitle))
        }
        
        # remove legend (if selected)
        if (input$addDescription == FALSE) {  
          p1 <- p1 + theme(legend.position = "none")
        }
        
        # remove gridlines (if selected)
        if (input$noGrid == TRUE) {  
          p1 <- p1 + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            )
        }
        
        if (!is.null(input$adjustColors) && input$adjustColors > 1) {
          p1 <- p1 + scale_color_manual(values = newColors)
          p1 <- p1 + scale_fill_manual(values = newColors)
        }
        
        # add facet wrap for category?
        if (!is.null(resultSummaryAxisYCat())) {
          p1 <- p1 + facet_grid(.~get(resultSummaryAxisYCat())) +
            theme(
              strip.background = element_rect(fill = NA, color = "black", size = 2),
              strip.text.x = element_text(color = "black")
            )
        }
        
        # facets for pop interaction?
        if (length((resultParamsPopsInt())) > 0) {
          p1 <- p1 + facet_grid(.~int.pop) +
            theme(
              strip.background = element_rect(fill = NA, color = "black", size = 2),
              strip.text.x = element_text(color = "black")
            )
        }
        
        # show titles?
        if (input$showFacetTitles == FALSE) {
          p1 <- p1 + theme(strip.text.x = element_blank())
        }
        
        # add axis titles
        p1 + xlab(xlabTitle) + ylab(ylabTitle) +
          theme(
            # hide legend title
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal",
            # line thickness
            axis.line = element_line(colour = "black", size = 1),
            panel.border = element_blank(),
            axis.ticks = element_line(colour = "black", size = 1)
            )
      }) %>% debounce(cciaConf()$tasks$results$poll)
      
      # selected ccia object
      cciaObj <- reactive({
        # moduleManagers()$imageViewerManager$shownImage()
        initCciaObject(pID = pID, uID = selectedUIDs()[[1]], versionID = versionID)()
      })
      
      # selected ccia set
      cciaSet <- reactive({
        # moduleManagers()$imageSetManager$selectedSet()
        initCciaObject(pID = pID, uID = uID, versionID = versionID)()
      })
      
      ### Observers - RxAction
      ## Event specific
      
      # add interaction column to experimental info
      observeEvent(c(
        expInfo(),
        resultSummaryAxisXList()
      ), {
        req(expInfo())
        req(resultSummaryAxisXCompiled())
        req(resultSummaryAxisXList())
        
        # get compiled info
        if (length(resultSummaryAxisXList()) > 1) {
          # https://stackoverflow.com/a/18360123
          expInfo()[, (resultSummaryAxisXCompiled()) := paste(
            get(resultSummaryAxisXList()[[1]]),
            get(resultSummaryAxisXList()[[2]]),
            sep = ".")]
        }
        
        expInfoUpdated(runif(1))
      })
      
      # listen to image selection
      # observeEvent(moduleManagers()$selectionManager$selectedUIDs(), {
      observeEvent(c(
        selectedUIDs(),
        popType(),
        resultParamsPops(),
        resultParamsPopsInt()
        ), {
        # req(moduleManagers()$selectionManager$selectedUIDs())
        req(selectedUIDs())
        req(popType())
        req(resultParamsPops())
        
        # make sure that it only requests data when selected
        req(globalManagers$input$sidebar() == session$ns(c()))
        
        progress <- Progress$new()
        progress$set(message = "Get population data", value = 50)
        
        # popDT(moduleManagers()$imageSetManager$selectedSet()$popDT(
        DT <- cciaSet()$popDT(
          popType = popType(),
          uIDs = selectedUIDs(),
          includeFiltered = TRUE,
          completeDT = TRUE,
          replaceNA = TRUE,
          pops = resultParamsPops()
        )
        
        # create population interactions
        if (length(resultParamsPopsInt()) > 0) {
          # get populations
          intDTs <- list()
            
          for (x in resultParamsPopsInt()) {
            # split population
            intPop <- stringr::str_split_fixed(x, pattern = "\\.", n = 2)
            
            # get population
            intDTs[[x]] <- cciaSet()$popDT(
              popType = intPop[[1]],
              uIDs = selectedUIDs(),
              includeFiltered = TRUE,
              completeDT = TRUE,
              replaceNA = TRUE,
              pops = intPop[[2]],
              popCols = c("label")
            )[, pop := NULL]
          }
          
          # bind together
          # TODO this assumes that each label has only one population
          intDT <- rbindlist(intDTs, idcol = "pop")
          
          # create pop interactions
          DT[intDT[, c("label", "pop")], on = c("label"),
             int.pop := i.pop, nomatch = NULL]
          
          # drop na
          # TODO should this be optional?
          DT <-  DT[complete.cases(DT[, "int.pop"]),]
        } else {
          # create dummy for interaction
          DT[, int.pop := NA]
        }
        
        # set whether multiple pops
        if (length(unique(DT$pop)) > 1)
          multiplePops(TRUE)
        else
          multiplePops(FALSE)
        
        # set unique ID
        numUIDs(length(unique(DT$uID)))
        
        progress$close()
        
        popDT(DT)
      })
      
      # create points DT
      observeEvent(c(popDT(), resultSummaryAxisY(), expInfoUpdated()), {
        req(popDT())
        req(resultSummaryAxisY())
        req(expInfo())
        
        # get mean of values for each image
        if (resultSummaryAxisY() %in% c("pop.freq")) {
          # create population frequencies
          # TODO data.table only version
          pointsDT(
            as.data.table(
              popDT()[, .(n1 = .N), by = .(uID, int.pop, pop)] %>%
                dplyr::group_by(uID) %>%
                mutate(pop.freq = n1/sum(n1) * 100) %>%
                ungroup() %>%
                complete(uID, int.pop, pop, fill = list(pop.freq = 0)) %>%
                dplyr::left_join(expInfo()))
          )
          
          # pointsDT()[, pop.freq := .SD$n1/sum(.SD$n1), by = .(uID)]
        } else {
          if (.cciaStatsTypeIsCategorical(resultSummaryAxisY())) {
            # TODO data.table only version 
            pointsDT(as.data.table(
              popDT() %>%
                dplyr::group_by(uID, int.pop, pop, get(resultSummaryAxisY())) %>%
                summarise(n1 = n()) %>%
                mutate(freq = n1/sum(n1) * 100) %>%
                dplyr::rename_with(
                  ~ c(paste0(resultSummaryAxisY(), ".cat"), resultSummaryAxisY()),
                  all_of(c("get(resultSummaryAxisY())",  "freq"))
                  ) %>%
                dplyr::left_join(expInfo())
              ))
          } else {
            pointsDT(
              popDT()[, c(
                n1 = .N,
                # TODO this is a helper column for summary is there a better way?
                sapply(.SD, function(x) list(x = mean(x, na.rm = TRUE))),
                sapply(.SD, function(x) list(mean = mean(x, na.rm = TRUE))),
                sapply(.SD, function(x) list(median = median(x, na.rm = TRUE))),
                sapply(.SD, function(x) list(sd = sd(x, na.rm = TRUE))),
                sapply(.SD, function(x) list(sem = sd(x, na.rm = TRUE) / sqrt(.N - 1))),
                sapply(.SD, function(x) list(min = min(x))),
                sapply(.SD, function(x) list(max = max(x))),
                sapply(.SD, function(x) list(meanCiLo = mean(x, na.rm = TRUE) + qt((1 - confidenceLevel)/2, .N - 1) * sd(x, na.rm = TRUE) / sqrt(.N - 1))),
                sapply(.SD, function(x) list(meanCiHi = mean(x, na.rm = TRUE) - qt((1 - confidenceLevel)/2, .N - 1) * sd(x, na.rm = TRUE) / sqrt(.N - 1))),
                sapply(.SD, function(x) list(medianCiLo = quantile(x, confidenceLevel, na.rm = TRUE))),
                sapply(.SD, function(x) list(medianCiHi = quantile(x, (1 - confidenceLevel), na.rm = TRUE)))
              ),
              .SDcols = resultSummaryAxisY(), by = .(uID, int.pop, pop)] %>%
                dplyr::left_join(expInfo())
            )
          }
          
          # rename columns back
          # TODO could that be done in DT?
          for (x in resultSummaryAxisY()) {
            data.table::setnames(pointsDT(), paste0(x, ".x"), x, skip_absent = TRUE)
          }
        } 
      })
      
      # create summary DT
      observeEvent(c(pointsDT(), resultSummaryAxisY(), expInfoUpdated()), {
        req(pointsDT())
        req(resultSummaryAxisY())
        req(resultSummaryAxisXCompiled())
        
        # make summary
        # https://stackoverflow.com/a/43834005
        summaryDT(
          pointsDT()[, c(
            n = .N,
            sapply(.SD, function(x) list(mean = mean(x, na.rm = TRUE))),
            sapply(.SD, function(x) list(median = median(x, na.rm = TRUE))),
            sapply(.SD, function(x) list(sd = sd(x, na.rm = TRUE))),
            sapply(.SD, function(x) list(sem = sd(x, na.rm = TRUE) / sqrt(.N - 1))),
            sapply(.SD, function(x) list(min = min(x))),
            sapply(.SD, function(x) list(max = max(x))),
            sapply(.SD, function(x) list(meanCiLo = mean(x, na.rm = TRUE) + qt((1 - confidenceLevel)/2, .N - 1) * sd(x, na.rm = TRUE) / sqrt(.N - 1))),
            sapply(.SD, function(x) list(meanCiHi = mean(x, na.rm = TRUE) - qt((1 - confidenceLevel)/2, .N - 1) * sd(x, na.rm = TRUE) / sqrt(.N - 1))),
            sapply(.SD, function(x) list(medianCiLo = quantile(x, confidenceLevel, na.rm = TRUE))),
            sapply(.SD, function(x) list(medianCiHi = quantile(x, (1 - confidenceLevel), na.rm = TRUE)))
            ), 
            .SDcols = resultSummaryAxisY(),
            by = eval(c(
              "int.pop", "pop", resultSummaryAxisXCompiled(), resultSummaryAxisYCat()
              ))]
        )
      })
      
      ## Generic
      
      ### UI Outputs
      ## Tables
      
      ## Plots
      
      # plot data
      output$plotData <- renderUI({
        # req(popType())
        req(cciaObj())
        
        # get pop type columns
        popTypePops <- list()
        popTypeCols <- list()
        popCats <- list()
        
        if (!is.null(popType())) {
          popTypePops <- unname(cciaObj()$popPaths(popType(), includeFiltered = TRUE))
          popTypeCols <- cciaObj()$labelPropsCols(popType = popType())
        }
        
        if (!is.null(popDT())) {
          popCats <- resultParamsCats()
        }
          
        popTypeChoices <- cciaConf()$parameters$popTypes
        
        # get choices for categories
        
        # create ui elements
        tagList(fluidRow(
          column(
            3,
            tags$label("Parameter plots"),
            selectInput(
              session$ns("resultParamsPopType"), "Population Type",
              choices = .reverseNamedList(popTypeChoices),
              # selected = isolate(popType())
              # selected = "live"
              # selected = "clust"
              selected = "flow"
              # selected = "region"
            ),
            createSelectInput(
              session$ns("resultParamsPops"),
              label = "Populations to get",
              choices = popTypePops,
              multiple = TRUE,
              # selected = isolate(resultParamsPops())
              # selected = c("OTI/tracked", "gBT/tracked")
              # selected = c("gBT+", "gBT+/clustered")
              selected = c("/nonDebris/P14/clustered",
                           "/nonDebris/P14/non.clustered")
              # selected = c("XCR1", "B", "O")
            ),
            createSelectInput(
              session$ns("resultParamsPopsShow"),
              label = "Populations to show",
              choices = popTypePops,
              multiple = TRUE,
              selected = isolate(resultParamsPopsShow())
            ),
            createSelectInput(
              session$ns("resultParamsPopsInt"),
              label = "Populations to interact",
              choices = popsAll(),
              multiple = TRUE,
              # selected = isolate(resultParamsPopsInt())
              selected = c("region.XCR1", "region.B", "region.O")
              # selected = c("flow./nonDebris/P14/clustered",
              #              "flow./nonDebris/P14/non.clustered")
            ),
            createSelectInput(
              session$ns("resultParamsCatsShow"),
              label = "Categories to show",
              choices = popCats,
              multiple = TRUE,
              selected = isolate(resultParamsCatsShow())
            )
            # createSelectInput(
            #   session$ns("resultParamsCols"),
            #   label = "Properties",
            #   choices = unname(cciaObj()$labelPropsCols()),
            #   multiple = TRUE,
            #   selected = isolate(resultParamsCols())
            # )
          ),
          column(
            3,
            tags$label("Summary plots"),
            createSelectInput(
              session$ns("resultSummaryAxisX"),
              label = "X Axis",
              choices = c("pop", colnames(expInfo())),
              multiple = FALSE,
              selected = isolate(resultSummaryAxisX())
              # selected = "Genotype"
            ),
            createSelectInput(
              session$ns("resultSummaryAxisY"),
              label = "Y Axis",
              choices = c("pop.freq", popTypeCols),
              multiple = FALSE,
              selected = isolate(resultSummaryAxisY())
              # selected = "live.cell.hmm.state.movement"
              # selected = "clust.cell.contact#clust.TRITC+"
            ),
            createSelectInput(
              session$ns("resultSummaryInteraction"),
              label = "Interaction",
              choices = c("NONE", colnames(expInfo())),
              multiple = FALSE,
              selected = isolate(resultSummaryInteraction())
            )
          )
        ))
      })
      
      # plot params
      output$plotParams <- renderUI({
        tagList(
          radioButtons(session$ns("jitterType"), "Data offset", choices = list(
            "Quasirandom" = "quasirandom", 
            # "Sinaplot" = "sina",
            "Random" = "random", 
            # "None; stripes" = "stripes",
            "None (for small n)" = "none"),
            selected = "quasirandom"),
          
          sliderInput(session$ns("alphaInput"), "Visibility of data", 0, 1, 1.0),
          
          radioButtons(session$ns("summaryInput"), "Statistics", choices = list(
            "Median" = "median",
            "Mean" = "mean",
            "Boxplot" = "box",
            "Violin Plot" = "violin"),
            selected = "violin"),
          
          checkboxInput(session$ns("addCI"),
                        label = HTML("Add 95% CI <br/>"),
                        value = TRUE),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == true && input['%s'] != 'box'",
                                session$ns("addCI"), session$ns("summaryInput")),
            checkboxInput(session$ns("errorBars"), label = "Classic error bars", value = FALSE)),
          
          conditionalPanel(
            condition = sprintf("input['%1$s'] == 'median' || input['%1$s'] == 'mean'", session$ns("summaryInput")),
            checkboxInput(session$ns("addBar"), label = HTML("Add a box that shows the range"), value = FALSE)),
          
          sliderInput(session$ns("alphaInputSum"), "Visibility of the statistics", 0, 1, 1),
          
          radioButtons(session$ns("ordered"), label = "Order of the conditions:", choices = list(
            "As supplied" = "none",
            "By median value" = "median",
            "By alphabet/number" = "alphabet"),
            selected = "none"),
          
          h4("Plot Layout"),      
          
          checkboxInput(session$ns("rotatePlot"),
                        label = "Rotate plot 90 degrees",
                        value = FALSE),
          
          checkboxInput(session$ns("noGrid"),
                        label = "Remove gridlines",
                        value = TRUE),
          
          checkboxInput(session$ns("changeScale"),
                        label = "Change scale",
                        value = FALSE),
          conditionalPanel(condition = sprintf("input['%s'] == true", session$ns("changeScale")),
                           checkboxInput(session$ns("scaleLog10"),
                                         label = "Log scale",
                                         value = FALSE),
                           
                           textInput(session$ns("range"), "Range of values (min,max)", value = "")),
          
          checkboxInput(session$ns("colorData"), "Use color for the data", value = FALSE),
          checkboxInput(session$ns("colorStats"), "Use color for the stats", value = FALSE),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == true || input['%s'] == true",
                                session$ns("colorData"), session$ns("colorStats")),
            radioButtons("adjustColors", "Color palette:", choices = list(
              "Standard" = 1,
              "Okabe&Ito; CUD" = 6,
              "Tol; bright" = 2,
              "Tol; muted" = 3,
              "Tol; light" = 4,
              "User defined" = 5),
              selected =  6),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 5", session$ns("adjustColors")),
              textInput(
                session$ns("userColorList"),
                "Names or hexadecimal codes separated by a comma (applied to conditions in alphabetical order):", 
                value = "turquoise2,#FF2222,lawngreen"), 
              
              h5("", a("Click here for more info on color names",
                       href = "https://r-charts.com/colors/", target = "_blank"))
            )),
          
          checkboxInput(session$ns("darkTheme"), label = "Dark Theme", value = FALSE),
          numericInput(session$ns("plotHeight"), "Height (# pixels): ", value = 480),
          numericInput(session$ns("plotWidth"), "Width (# pixels):", value = 480),
          
          h4("Labels/captions"),
          
          checkboxInput(session$ns("addTitle"),
                        label = "Add title",
                        value = FALSE),
                        
          conditionalPanel(
            condition = sprintf("input['%s'] == true", session$ns("addTitle")),
            textInput(session$ns("title"), "Title:", value = "")
          ),
          
          checkboxInput(session$ns("labelAxes"),
                        label = "Change labels",
                        value = FALSE),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == true", session$ns("labelAxes")),
            textInput(session$ns("labX"), "X-axis:", value = ""),
            textInput(session$ns("labY"), "Y-axis:", value = "")),
          
          checkboxInput(session$ns("adjFontSize"),
                        label = "Change font size",
                        value = FALSE),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == true", session$ns("adjFontSize")),
            numericInput(session$ns("adjFontSizeAxTitle"), "Size axis titles:", value = 24),
            numericInput(session$ns("adjFontSizeAxLabels"), "Size axis labels:", value = 18)),
          checkboxInput(session$ns("addDescription"),
                        label = "Add figure description",
                        value = TRUE),
          
          checkboxInput(session$ns("showFacetTitles"),
                        label = "Show facet titles",
                        value = TRUE)
        )
      })
      
      # plot output
      output$plotOutput <- renderUI({
        req(numUIDs() > 0)
        
        tagList(
          fluidRow(
            downloadButton(ns("downloadPlotPDF"), "Download pdf-file"),
            downloadButton(ns("downloadPlotSVG"), "Download svg-file"), 
            downloadButton(ns("downloadPlotEPS"), "Download eps-file"), 
            downloadButton(ns("downloadPlotPNG"), "Download png-file"),
            downloadButton(ns("downloadPlotCSV"), "Download csv-file")
          ),
          br(),
          fluidRow(
            tabsetPanel(
              id = ns("plotOutputTabs"),
              selected = "combined",
              tabPanel(
                "Combined", value = "combined",
                plotOutput(ns("plotOutputCombined"), height = "400px")
              ),
              tabPanel(
                "Individual", value = "individual",
                plotOutput(ns("plotOutputIndv"),
                           height = paste0(floor(numUIDs()/6) * 400, "px"))
              )
            )
          )
        )
      })
      
      # combined plots
      output$plotOutputCombined <- renderPlot(width = plotWidth, height = plotHeight, {
        req(summaryPlotData())
        
        plot(summaryPlotData())
      })
      
      # individual image plots
      output$plotOutputIndv <- renderPlot(width = plotWidth, height = plotHeight, {
        req(indvPlotData())
        
        plot(indvPlotData())
      })
      
      ## Buttons
      output$downloadPlotPDF <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".pdf", sep = "")
        },
        content <- function(file) {
          pdf(file, width = plotWidth()/72, height = plotHeight()/72)
          plot(plotData())
          dev.off()
        },
        contentType = "application/pdf" # MIME type of the file
      )
      
      output$downloadPlotSVG <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".svg", sep = "")
        },
        content <- function(file) {
          svg(file, width = plotWidth()/72, height = plotHeight()/72)
          plot(plotData())
          dev.off()
        },
        contentType = "application/svg" # MIME type of the file
      )
      
      output$downloadPlotEPS <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".eps", sep = "")
        },
        content <- function(file) {
          cairo_ps(file, width = plotWidth()/72, height = plotHeight()/72)
          plot(plotData())
          dev.off()
          
        },
        contentType = "application/eps" # MIME type of the file
      )
      
      output$downloadPlotPNG <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".png", sep = "")
        },
        content <- function(file) {
          png(file, width = plotWidth()*4, height = plotHeight()*4, res = 300)
          plot(plotData())
          dev.off()
        },
        contentType = "application/png" # MIME type of the file
      )
      
      output$downloadPlotCSV <- downloadHandler(
        filename <- function() {
          paste("cciaPlot", Sys.time(), ".csv", sep = "")
        },
        content <- function(file) {
          write.csv(pointsData(), file)
        },
        contentType = "text/csv" # MIME type of the file
      )
      
      ## Other
    }
  )
}

shinyApp(ui = ui, server = server)
