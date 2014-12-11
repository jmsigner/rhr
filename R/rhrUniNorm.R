##' Univariate Normal Home-Range estimation 
##'
##' Computes home-range using univariate normal distribution
##' @title rhrUniNorm
##' @param xy valid input data 
##' @param trast template raster
##' @export
rhrUniNorm <- function(xy, trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {
                       
  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  ## check input 
  projString <- if (inherits(xy, "SpatialPoints")) {
    proj4string(xy) 
  } else if (is(xy, "RhrMappedData")) {
    proj4string(xy$dat)
  } else {
    CRS(NA_character_)
  }
  xy <- rhrCheckData(xy, returnSP=FALSE)
  
  ll <- function(xy, par) -sum(mvtnorm::dmvnorm(xy, mean=c(par[1], par[2]),
                                                sigma=matrix(c(par[3], par[4], par[4], par[5]),
                                                  nrow=2, byrow=TRUE),
                                                log=TRUE))

  phat <- optim(par=c(mean(xy[, 1]), mean(xy[, 2]), 1, 0.1, 1), xy=xy, fn=ll)$par

  ll <- sum(mvtnorm::dmvnorm(xy, mean=c(phat[1:2]),
                             sigma=matrix(phat[c(3, 4, 4, 5)], byrow=2, nrow=2), log=TRUE))

  ## ud
  if (is.null(trast)) {
    trast <- rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)
  }

  r1 <- data.frame(rasterToPoints(trast))
  r1$density <- dUniNorm(r1[, 1:2], mu=c(phat[1:2]),
                         sigma=matrix(phat[c(3, 4, 4, 5)], byrow=2, nrow=2))
  ud <- rasterFromXYZ(r1)

  ## project ud
  proj4string(ud) <- projString

  ## AIC
  K <- 6
  AIC <- -2 * ll + 2 * K
  AICc <- AIC + (2*K*(K+1)) / (nrow(xy) - K -1)

  res <- structure(
    list(
      args=args,
      call=call,
      model="Unimodal Normal", 
      K=K,
      LL=ll, 
      AIC=AIC, 
      AICc=AICc, 
      ud=ud,
      parameters=list(
        mean=phat[1:2], 
        sigma=matrix(phat[c(3, 4, 4, 5)], byrow=2, nrow=2)
        )),
    class=c("RhrUniNorm", "RhrProbEst", "RhrEst", "list"))
  return(invisible(res))
}

dUniNorm <- function(xy, mu, sigma) {
  mvtnorm::dmvnorm(xy, mean=mu, sigma=sigma)
}

##' @export
rhrUD.RhrUniNorm <- function(x, ...) {
  x$ud
}

##' @export
rhrCUD.RhrUniNorm <- function(x, ...) {
  r1 <- rhrUD(x)
  rhrUD2CUD(r1)
}

##' @export
rhrIsopleths.RhrUniNorm <- function(x, levels=95, ...) {
  cud <- rhrCUD(x)
  rhrCUD2Isopleths(cud, levels)
}

##' @export
rhrArea.RhrUniNorm <- function(x, levels=95, ...) {
  as.data.frame(rhrIsopleths(x, levels))
}

##' @export
##' @rdname rhrHasUD
rhrHasUD.RhrUniNorm <- function(x, ...) {
  TRUE
}

##' @export
rhrData.RhrUniNorm <- function(x, spatial=FALSE, ...) {
  xx <- rhrCheckData(x$args$xy, returnSP=spatial)
}

##' @export
##' @method plot RhrUniNorm
plot.RhrUniNorm <- function(x, levels=95, ...) {
  ud <- rhrUD(x)
  iso <- rhrIsopleths(x, levels)
  plot(ud)
  plot(iso, add=TRUE)
}
