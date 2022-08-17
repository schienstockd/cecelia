#' @depcrecated
#' I keep these methods in case this might help to visualise images loaded from
#' Zarr/R. This might be helpful for plotting figures and graphs. It will
#' not be as efficient as other methods; so, maybe this should rather be
#' in a python wrapper that is called to produce figures for reports.
#' 
#' @description Change image colour
#' @param im magickImage
#' @param imColour character to colour image
#' @examples
#' TODO
.changeImageColour <- function(im, imColour){
  # multiply image for colour
  dummyIm <- image_colorize(im, 100, imColour)
  im <- image_convert(im, type = "TrueColor")
  im <- image_composite(
    im, dummyIm, operator = "Multiply")
  
  # the official way is this:
  # https://github.com/ropensci/magick/issues/250
  # That is too slow and times out
  # gr <- c(
  #   image_blank(10, 33, pseudo_image = paste0("gradient:black-", imColour))
  # ) %>% image_append(stack=TRUE)
  # image_fx_sequence(c(im, gr), 'v.p{0,u*v.h}')
  
  im
}

#' @description Change image colour
#' @param imList list of magickImage
#' @param imColours list of character to colour image
#' @examples
#' TODO
.changeImageColours <- function(imList, imColours){
  # go through images and change colours
  for (curIm in seq(1, length(imList))){
    imList[[curIm]] <- changeImageColour(imList[[curIm]], imColours[curIm])
  }
  
  imList
}

#' @description Create composite image
#' @param imList list of magickImage
#' @examples
#' TODO
.createComposite <- function(imList){
  curComposite <- imList[[1]]
  
  for (i in seq(1, length(imList) - 1)){
    curComposite <- image_composite(
      curComposite, imList[[i + 1]], operator = "blend")
  }
  
  curComposite
}