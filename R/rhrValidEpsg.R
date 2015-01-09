##' Validate EPSG code
##'
##' Takes an EPSG checks if it is valid
##' @title rhrValidEpsg
##' @param epsg EPSG code
##' @export
rhrValidEpsg <- function(epsg) {
  rhrEPSGs <- rgdal::make_EPSG()
  if (is.null(epsg)) {
    return(FALSE)
  }
  if (as.character(epsg) %in% rhrEPSGs$code) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


