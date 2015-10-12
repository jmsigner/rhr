##' Bivariate Unimodal Normal Home-Range estimation 
##'
##' Computes home range using bivariate bimodal normal distribution.
##' @template xy
##' @template trast
##' @export
rhrUniNorm <- function(xy, trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {
                       
  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  ## check input 
  projString <- if (inherits(xy, "SpatialPoints")) {
    sp::proj4string(xy) 
  } else if (is(xy, "RhrMappedData")) {
    sp::proj4string(xy$dat)
  } else {
    sp::CRS(NA_character_)
  }
  xy <- rhrCheckData(xy, returnSP=FALSE)
  
  ll <- function(xy, par) -sum(mvtnorm::dmvnorm(xy, mean=c(par[1], par[2]),
                                                sigma=matrix(c(par[3], par[4], par[4], par[5]),
                                                  nrow=2, byrow=TRUE),
                                                log=TRUE))

  phat <- optim(par=c(mean(xy[, 1]), mean(xy[, 2]), 1, 0.1, 1), xy=xy, fn=ll)$par

  ll <- sum(mvtnorm::dmvnorm(xy, mean=c(phat[1:2]),
                             sigma=matrix(phat[c(3, 4, 4, 5)], byrow=2, nrow=2), log=TRUE))

  r1 <- data.frame(raster::rasterToPoints(trast))
  r1$density <- dUniNorm(r1[, 1:2], mu=c(phat[1:2]),
                         sigma=matrix(phat[c(3, 4, 4, 5)], byrow=2, nrow=2))
  ud <- raster::rasterFromXYZ(r1)

  ## project ud
  sp::proj4string(ud) <- projString

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
    class=c("RhrUniNorm", "RhrParamEst", "RhrProbEst", "RhrEst", "list"))
  return(invisible(res))
}

dUniNorm <- function(xy, mu, sigma) {
  mvtnorm::dmvnorm(xy, mean=mu, sigma=sigma)
}

