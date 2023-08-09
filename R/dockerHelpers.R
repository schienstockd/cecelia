# map path back to host system
.dockerMapPathToHost <- function(filePath, path = "projects") {
  if (cciaConf()$docker$useDocker == TRUE) {
    # replace mapping with defined path
    for (i in length(cciaConf()$docker$pathMapping$from)) {
      filePath <- stringr::str_replace(
        filePath,
        cciaConf()$docker$pathMapping$from[[path]],
        cciaConf()$docker$pathMapping$to[[path]])
    }
  }
  
  filePath
}
