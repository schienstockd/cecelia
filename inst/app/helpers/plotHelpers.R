#' @description Input fields to format plot
#' @param session shiny session
#' @examples
#' TODO
#' @export
# summary data for plot
.formatSummaryPlotDataInputs <- function(session) {
  tagList(
    h4("Plot Layout"),      
    
    checkboxInput(session$ns("rotatePlot"),
                  label = "Rotate plot 90 degrees",
                  value = FALSE),
    
    checkboxInput(session$ns("rotateXLabel"),
                  label = "Rotate X axis 45 degrees",
                  value = TRUE),
    
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
}
  
#' @description Format summary plot based on inputs
#' @param p1 ggplot plot
#' @param input shiny input values
#' @param xlabTitle character for X-axis label
#' @param ylabTitle character for Y-axis label
#' @examples
#' TODO
#' @export
# summary data for plot
.formatSummaryPlotData <- function(p1, input, xlabTitle = "", ylabTitle = "") {
  widthColumn <- 0.7
  
  # Change linecolor in case of dark mode
  if (input$darkTheme) {
    lineColor <- "grey80"
  } else if (input$darkTheme == FALSE) {
    lineColor <- "black"
  } 
  
  # format layout
  p1 <- p1 + theme_light(base_size = 16)
  if (input$darkTheme) {p1 <- p1 + theme_darker(base_size = 16)}
  
  # adjust scale if range (min, max) is specified
  if (input$range != "" &&  input$changeScale == TRUE) {
    rng <- as.numeric(strsplit(input$range,",")[[1]])
    
    # if min > max invert the axis
    if (rng[1] > rng[2]) {p1 <- p1 + scale_y_reverse()}
    
    # autoscale if rangeis NOT specified
  } else if (input$range == "" || input$changeScale == FALSE) {
    rng <- c(NULL, NULL)
  }
  
  p1 <- p1 + coord_cartesian(ylim = c(rng[1], rng[2]))
  
  # If selected, rotate plot 90 degrees C
  if (input$rotatePlot == TRUE) {
    p1 <- p1 + coord_flip(ylim = c(rng[1], rng[2]))
  }
  
  # If selected, rotate x label by 45 degrees C
  if (input$rotateXLabel == TRUE) {
    p1 <- p1 + theme(axis.text.x = element_text(
      angle = 45, hjust = 1, vjust = 1))
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
  
  # show titles?
  if (input$showFacetTitles == FALSE) {
    p1 <- p1 +
      theme(
        strip.text.x = element_blank()
      )
  }
  
  # add axis titles
  p1 <- p1 + xlab(xlabTitle) + ylab(ylabTitle) +
    theme(
      # hide legend title
      legend.title = element_blank(),
      # legend.position = "right",
      # legend.direction = "vertical",
      legend.position = "bottom",
      legend.direction = "horizontal",
      # line thickness
      axis.line = element_line(colour = "black", size = 1),
      panel.border = element_blank(),
      axis.ticks = element_line(colour = "black", size = 1)
    )
  
  # add further details to p1
  p1
}