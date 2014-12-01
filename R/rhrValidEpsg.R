##' @export
rhrValidEpsg <- function(epsg) {
  data(rhrEPSGs)
  if (is.null(epsg)) {
    return(FALSE)
  }
  if (epsg %in% rhrEPSGs) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
