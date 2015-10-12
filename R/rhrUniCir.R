#' @export
#' @rdname parametric_homeranges
rhrUniCirc <- function(xy, trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {
  xy <- rhrCheckData(xy, returnSP=FALSE)

  ## estimate parameters
  nll <- function(xy, par) {
    if (par[3] < 1e-3 | par[4] < 1e-3) {
      1e250
    } else {
      -sum(log(dbexppm(xy, par[1], par[2], par[3], par[4])))
    }
  }
  
  
  ## AIC
  phat <- optim(par=c(mean(xy[, 1]), mean(xy[, 2]), 1, 1), xy=xy, fn=nll)$par
  ll <- sum(log(dbexppm(xy[, 1:2], phat[1], phat[2], c=phat[3], a=phat[4])))
  
  
  # ud
  r1 <- data.frame(raster::rasterToPoints(trast))
  r1$density <- dbexppm(r1[, 1:2], phat[1], phat[2], phat[3], phat[4])
  ud <- raster::rasterFromXYZ(r1)


  # AIC
  K <- 4
  AIC <- -2 * ll + 2 * K

  # AICc
  AICc <- AIC + (2*K*(K+1)) / (nrow(xy) - K -1)

  res <- structure(
    list(
    model="Unimodal Circular", 
    K=K,
    LL=ll, 
    AIC=AIC, 
    AICc=AICc, 
    ud = ud,
    parameters=list(
      mean=phat[1:2],
      c=phat[3],
      a=phat[4])),
    class=c("RhrUniCirc", "RhrParamEst", "RhrProbEst", "RhrEst", "list"))
  return(invisible(res))
}

dbexppm <- function(xy, mux, muy, c, a) {
  (2 / (c * 2 * pi * a^2 * gamma(c))) * exp(-(sqrt((xy[, 1] - mux)^2 + (xy[, 2] - muy)^2)/a)^(2/c))
}



