# create styled colours
createStyledColourChoices <- function(colPalette){
  initColours <- paste0("color:", colPalette, ";")
  initColours <- paste0(initColours, "background:", colPalette ,";")
  initColours <- paste0(initColours, "font-weight: bold;")

  return(initColours)
}

