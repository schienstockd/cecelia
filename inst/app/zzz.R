#' @description Create app
cciaCreateApp <- function() {
  # copy all files to project directory
  browser()
}

# run app with port
cciaRunApp <- function() {
  shiny::runApp(system.file(".", package = "ceceliaApp"))
}