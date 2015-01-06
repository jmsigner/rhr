##' Validate EPSG code
##'
##' Takes an EPSG checks if it is valid
##' @title rhrValidEpsg
##' @param epsg EPSG code
##' @export
rhrValidEpsg <- function(epsg) {
  rhrEPSGs <- NULL
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
