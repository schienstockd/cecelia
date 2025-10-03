#' @description
#' Convert list to numeric; https://stackoverflow.com/a/29819032/13766165
#' @param l list of generic to convert
#' @examples
#' TODO
#' @export
list.as.numeric <- function(l) {
  lapply(l, function(x)
    if (is.list(x))
      list.as.numeric(x)
    else
      suppressWarnings({
        if (!all(is.na(as.numeric(x))))
          as.numeric(x)
        else
          x
      })
  )
}