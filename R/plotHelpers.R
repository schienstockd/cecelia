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
    axis.ticks = element_line(colour = "white")
  )
}