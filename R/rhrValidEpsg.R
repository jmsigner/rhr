##' @export
rhrValidEpsg <- function(epsg) {
  data(rhrEPSGs, envir=environment())
  if (is.null(epsg)) {
    return(FALSE)
  }
  if (epsg %in% rhr:::rhrEPSGs) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
