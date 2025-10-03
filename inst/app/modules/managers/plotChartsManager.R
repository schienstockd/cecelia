# manage populations from gating or mapping
createPlotChartsManager <- function(
    input, output, session, globalManagers, moduleManagers, managerConf) {
  # set default parameters
  # if (!"enableAddPopulation" %in% names(managerConf$plotCharts)) {
  #   managerConf$plotCharts$enableAddPopulation <- FALSE
  # }
  
  ### Functions
  
  ### Reactive values
  
  ### Reactive-like values
  
  ### Reactives - RxCalc
  ## Event specific
  
  ## Generic
  
  # plot properties
  plotWidth <- reactive({
    input$plotWidth
  })
  
  plotHeight <- reactive({
    input$plotHeight
  })
  
  ### Observers - RxAction
  ## Event specific
  
  ## Generic
  
  ### UI Outputs
  ## Tables
  
  ## Plots
  
  # plot output
  output$plotOutput <- renderUI({
    tagList(
      fluidRow(
        downloadButton(session$ns("downloadPlotPDF"), "Download pdf-file"),
        downloadButton(session$ns("downloadPlotSVG"), "Download svg-file"), 
        downloadButton(session$ns("downloadPlotEPS"), "Download eps-file"), 
        downloadButton(session$ns("downloadPlotPNG"), "Download png-file"),
        downloadButton(session$ns("downloadPlotCSV"), "Download csv-file")
      ),
      br(),
      fluidRow(
        tabsetPanel(
          id = session$ns("plotOutputTabs"),
          selected = "combined",
          tabPanel(
            "Combined", value = "combined",
            plotOutput(session$ns("plotOutputCombined"), height = "400px")
          ),
          tabPanel(
            "Individual", value = "individual",
            plotOutput(session$ns("plotOutputIndv"),
                       height = paste0(floor(managerConf$plotCharts$numUIDs()/6) * 400, "px"))
          )
        )
      )
    )
  })
  
  # combined plots
  output$plotOutputCombined <- renderPlot(width = plotWidth, height = plotHeight, {
    req(managerConf$plotCharts$summaryPlotData())
    
    plot(managerConf$plotCharts$summaryPlotData())
  })
  
  # individual image plots
  output$plotOutputIndv <- renderPlot(width = plotWidth, height = plotHeight, {
    req("indvPlotData" %in% names(managerConf$plotCharts))
    req(managerConf$plotCharts$indvPlotData())
    
    plot(managerConf$plotCharts$indvPlotData())
  })
  
  ## Buttons
  output$downloadPlotPDF <- downloadHandler(
    filename <- function() {
      paste("cciaPlot", Sys.time(), ".pdf", sep = "")
    },
    content <- function(file) {
      pdf(file, width = plotWidth()/72, height = plotHeight()/72)
      plot(managerConf$plotCharts$plotData())
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
      plot(managerConf$plotCharts$plotData())
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
      plot(managerConf$plotCharts$plotData())
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
      plot(managerConf$plotCharts$plotData())
      dev.off()
    },
    contentType = "application/png" # MIME type of the file
  )
  
  output$downloadPlotCSV <- downloadHandler(
    filename <- function() {
      paste("cciaPlot", Sys.time(), ".csv", sep = "")
    },
    content <- function(file) {
      data.table::fwrite(managerConf$plotCharts$summaryData(), file)
    },
    contentType = "text/csv" # MIME type of the file
  )
  
  ## Other
  
  ## public functions
  list(
  )
}
