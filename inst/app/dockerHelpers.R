# map path back to host system
dockerMapPathToHost <- function(filePath) {
  if (USE_DOCKER == TRUE) {
    # replace mapping with defined path
    for (i in length(DOCKER_DATA_PATH_MAPPING$from)) {
      filePath <- str_replace(
        filePath,
        DOCKER_DATA_PATH_MAPPING$from[[1]],
        DOCKER_DATA_PATH_MAPPING$to[[1]])
    }
  }
  
  filePath
}