#' Bimodal circular home range estimation
#'
#' Computes home-range using two circular normal distributions
#' @title rhrBiCirc
#' @template xy
#' @template trast
#' @name parametric_homeranges
NULL

##' @export
rhrUD.RhrParamEst <- function(x, ...) {
  x$ud
}

##' @export
rhrCUD.RhrParamEst <- function(x, ...) {
  r1 <- rhrUD(x)
  rhrUD2CUD(r1)
}

##' @export
rhrIsopleths.RhrParamEst <- function(x, levels=95, ...) {
  cud <- rhrCUD(x)
  rhrCUD2Isopleths(cud, levels)
}

##' @export
rhrArea.RhrParamEst <- function(x, levels=95, ...) {
  as.data.frame(rhrIsopleths(x, levels))
}

##' @export
##' @rdname rhrHasUD
rhrHasUD.RhrParamEst <- function(x, ...) {
  TRUE
}

##' @export
rhrData.RhrParamEst <- function(x, spatial=FALSE, ...) {
  xx <- rhrCheckData(x$args$xy, returnSP=spatial)
}

##' @export
plot.RhrParamEst <- function(x, levels=95, ...) {
  ud <- rhrUD(x)
  iso <- rhrIsopleths(x, levels)
  plotRaster(ud)
  sp::plot(iso, add=TRUE)
}
