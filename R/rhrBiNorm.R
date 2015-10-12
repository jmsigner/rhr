#' Bivariate bimodal Normal Home-Range estimation 
#'
#' Computes home-range using bivariate bimodal normal distribution
#' @title rhrBiNorm
#' @param xy valid input data 
#' @param trast template raster
#' @param maxit Integer, giving the maximum number of iterations. See also \link{\code{mixtools::mvnormalmixEM}}, which is called.
#' @export
rhrBiNorm <- function(xy, trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL), 
                      maxit = 20) {

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

  hats <- mixtools::mvnormalmixEM(xy[, 1:2], k=2, maxit = maxit)

  r1 <- data.frame(raster::rasterToPoints(trast))
  r1$density <- d2mvnorm(r1[, 1:2], m=hats$lambda[1], 
                         mu1=hats$mu[[1]], sig1=hats$sigma[[1]], mu2=hats$mu[[2]], sig2=hats$sigma[[2]])

  ud <- raster::rasterFromXYZ(r1)

  ## log likelihood
  ll <- hats$loglik

  ## AIC(s)
  K <- 11
  AIC <- -2 * ll + 2 * K
  AICc <- AIC + (2*K*(K+1)) / (nrow(xy) - K -1)

  ## project
  sp::proj4string(ud) <- projString

  res <- structure(
    list(
      model="Bimodal Normal", 
      args=args, 
      call=call,
      K=K,
      LL=ll, 
      AIC=AIC, 
      AICc=AICc, 
      ud=ud,
      parameters=list(
        pi=hats$lambda[1], 
        mean1=hats$mu[[1]], 
        mean2=hats$mu[[2]],
        sigma1=hats$sigma[[1]], 
        sigma2=hats$sigma[[2]])),
    class=c("RhrBiNorm", "RhrParamEst", "RhrProbEst", "RhrEst", "list"))
  return(invisible(res))
}

d2mvnorm <- function(xy, m, mu1, sig1, mu2, sig2) {
  m * mvtnorm::dmvnorm(xy, mu1, sig1) + (1 - m) * mvtnorm::dmvnorm(xy, mu2, sig2)
}

