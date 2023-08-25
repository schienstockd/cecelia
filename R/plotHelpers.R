#' @description Collection of functions to create plots
#' 
#' @method 
#' \code{plotThemeBoxplot} to create a box plot
#' \code{plotThemeViolin} to create a violin plot
#' \code{plotThemeBar} to create a bar plot
#' \code{plotThemeDark} to create a dark plot
#' 
#' @param p1 ggplot object
#' @param angle numeric for X-axis angle
#' @param fontSize numeric for font size
#' @param legend.justification character for legend justification
#' 
#' @examples
#' TODO
#' @export
plotThemeBoxplot <- function(p1, ...) {
  # plot
  p1 + 
    theme_classic() +
    geom_boxplot(outlier.alpha = 0) +
    geom_jitter(
      position = position_jitterdodge(jitter.width = 0.10), alpha = 1.0,
      color = "white") +
    # theme(legend.title = element_blank()) +
    # scale_fill_discrete("") +
    scale_fill_brewer(name = NULL, palette = "Set3") +
    plotThemeDark(...)
}

#' @export
plotThemeViolin <- function(p1, ...) {
  # plot
  p1 +
    theme_classic() +
    geom_violin(scale = "width") +
    scale_fill_brewer(name = NULL, palette = "Set3") +
    plotThemeDark(...)
}

#' @export
plotThemeBar <- function(p1, ...) {
  # plot
  p1 +
    theme_classic() +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_brewer(name = NULL, palette = "Set3") +
    plotThemeDark(...)
}

#' @export
plotThemeDark <- function(angle = 30, fontSize = 15, legend.justification = "left") {
  # adjust vertical and horizontal
  if (angle > 0) {
    vjust = 1
    hjust = 1
  } else {
    vjust = 1/2
    hjust = 1/2
  }
  
  theme(
    legend.direction = "horizontal",
    legend.position = "top",
    legend.justification = legend.justification,
    legend.background = element_rect(fill = "#222222"),
    legend.text = element_text(
      colour = "white", size = fontSize),
    legend.title = element_text(
      colour = "white", size = fontSize),
    panel.background = element_rect(fill = "#222222"),
    plot.background = element_rect(
      fill = "#222222", color = "#222222"
    ),
    axis.text.x = element_text(
      angle = angle, vjust = vjust, hjust = hjust,
      colour = "white", size = fontSize),
    axis.text.y = element_text(
      colour = "white", size = fontSize
    ),
    axis.title.y = element_text(
      colour = "white", size = fontSize),
    axis.title.x = element_text(
      colour = "white", size = fontSize),
    axis.line = element_line(colour = "white"),
    axis.ticks = element_line(colour = "white"),
    strip.text = element_text(
      colour = "white", size = fontSize),
  )
}

# colour themes
# from Paul Tol: https://personal.sron.nl/~pault/
#' @examples
#' TODO
#' @export
.plotColourTolBright <- c("#EE6677", "#228833", "#4477AA", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB")
#' @examples
#' TODO
#' @export
.plotColourTolMuted <- c("#88CCEE", "#44AA99", "#117733", "#332288", "#DDCC77", "#999933","#CC6677", "#882255", "#AA4499", "#DDDDDD")
#' @examples
#' TODO
#' @export
.plotColourTolLight <- c("#BBCC33", "#AAAA00", "#77AADD", "#EE8866", "#EEDD88", "#FFAABB", "#99DDFF", "#44BB99", "#DDDDDD")

# from Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
.plotColourOkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Adapted from https://github.com/JoachimGoedhart/PlotsOfData
# TODO need to adjust the following
#' @examples
#' TODO
#' @export
theme_darker <- function(base_size = 11, base_family = "",  base_line_size = base_size/22, base_rect_size = base_size/22)
{
  half_line <- base_size/2
  theme_light() %+replace%
    theme(line = element_line(colour = "grey100", size = 0.5, linetype = 1, lineend = "butt"),
          
          rect = element_rect(fill = "grey0", colour = "grey100", size = 0.5, linetype = 1),
          
          text = element_text(family = base_family, face = "plain", colour = "grey100", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
          
          axis.line = element_line(),
          axis.line.x = element_blank(), 
          axis.line.y = element_blank(),
          
          axis.text = element_text(size = rel(0.8), colour = "grey80"),
          # axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
          # axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
          
          axis.ticks = element_line(colour = "grey20"), 
          # axis.ticks.length = unit(half_line/2, "pt"),
          
          # axis.title.x = element_text(margin = margin(t = 0.8 * half_line, b = 0.8 * half_line/2)),
          # axis.title.y = element_text(angle = 90, margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)),
          
          legend.background = element_rect(colour = NA),
          # legend.margin = unit(0.2, "cm"),
          legend.key = element_rect(fill = NULL, colour = "grey0"),
          # legend.key.size = unit(1.2, "lines"), 
          # legend.key.height = NULL, legend.key.width = NULL,
          # legend.text = element_text(size = rel(0.8)), 
          # legend.text.align = NULL,
          # legend.title = element_text(hjust = 0), 
          # legend.title.align = NULL,
          # legend.position = "right", 
          # legend.direction = NULL,
          # legend.justification = "center", 
          # legend.box = NULL,
          
          panel.background = element_rect(fill = "grey10", colour = NA),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "grey0"), 
          panel.grid.minor = element_line(colour = "grey0", size = 0.25), 
          # panel.margin = unit(half_line, "pt"),
          # panel.margin.x = NULL, 
          # panel.margin.y = NULL,
          panel.ontop = FALSE,
          
          strip.background = element_rect(fill = "grey40", colour = NA),
          strip.text = element_text(colour = "grey10", size = rel(0.8)),
          strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
          strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
          strip.switch.pad.grid = unit(0.1, "cm"),
          strip.switch.pad.wrap = unit(0.1, "cm"),
          
          #To avoid white line around plot panel
          plot.background = element_rect(colour = "black"), 
          # plot.title = element_text(size = rel(1.2), margin = margin(b = half_line * 1.2)),
          # plot.margin = margin(half_line, half_line, half_line, half_line),
          complete = TRUE)
  
}

#' @examples
#' TODO
#' @export
theme_light_dark_bg <- function(base_size = 11, base_family = "",
                                base_line_size = base_size / 22,
                                base_rect_size = base_size / 22) {
  half_line <- base_size / 2
  
  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      plot.background = element_rect(colour = "black", fill="black"), 
      text = element_text(family = base_family, face = "plain", colour = "grey100", size = base_size,
                          lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE) ,
      axis.text = element_text(size = rel(0.8),colour = "grey80"),
      
      
      # axis.text = element_text(colour = "grey80"),
      
      # white panel with light grey border
      panel.background = element_rect(fill = "grey5", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey80", size = rel(1)),
      # light grey, thinner gridlines
      # => make them slightly darker to keep acceptable contrast
      panel.grid       = element_line(colour = "grey30"),
      panel.grid.major = element_line(size = rel(0.5)),
      panel.grid.minor = element_line(size = rel(0.25)),
      
      # match axes ticks thickness to gridlines and colour to panel border
      axis.ticks       = element_line(colour = "grey80", size = rel(0.5)),
      
      # match legend key to plot.background
      legend.background = element_rect(fill= "black", colour = NULL),
      legend.key       = element_rect(fill = "black", colour = NULL),
      
      # dark strips with light text (inverse contrast compared to theme_grey)
      strip.background = element_rect(fill = "grey40", colour = NA),
      strip.text       = element_text(
        colour = "grey10",
        size = rel(0.8),
        margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
      ),
      
      complete = TRUE
    )
  
}



############## https://debruine.github.io/posts/plot-comparison/

bgcolor <- "black"
textcolor <- "white"


theme_debruine <- function(base_size = 11, base_family = ""){
  half_line <- base_size/2
  theme_light() %+replace%
    
    theme(line = element_line(colour = "grey100", size = 0.5, linetype = 1, lineend = "butt"),
          
          rect = element_rect(fill = "grey0", colour = "grey100", size = 0.5, linetype = 1),
          plot.background = element_rect(fill = bgcolor, colour = bgcolor),
          panel.background = element_rect(fill = NA),
          legend.background = element_rect(fill = NA),
          legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(family=base_family, colour = textcolor, size=20),
          axis.text = element_text(family=base_family, colour = textcolor, size=15),
          complete = TRUE)
}



######## https://emanuelaf.github.io/own-ggplot-theme.html
blue_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "blue", fill = NA, linetype = 2),
    # color background 2)
    panel.background = element_rect(fill = "aliceblue"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "steelblue", face = "italic", family = "Times New Roman"),
    axis.title = element_text(colour = "steelblue", family = "Times New Roman"),
    axis.ticks = element_line(colour = "steelblue"),
    # legend at the bottom 6)
    legend.position = "bottom"
  )
}