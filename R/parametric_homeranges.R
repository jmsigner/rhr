#' Bimodal circular home range estimation
#'
#' Computes home-range using two circular normal distributions
#' @title rhrBiCirc
#' @template xy
#' @template trast
#' @param maxit Integer, giving the maximum number of iterations. See also \link{\code{mixtools::mvnormalmixEM}}, which is called.
#' @name parametric_homeranges
NULL

#' @export
#' @rdname parametric_homeranges
rhrUniCirc <- function(xy, trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL), 
                       levels = 95) {
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


#' @export
#' @rdname parametric_homeranges
rhrUniNorm <- function(xy, trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL), 
                       levels = 95) {
                       
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


#' @export
#' @rdname parametric_homeranges
rhrBiCirc <- function(xy, trast=rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)) {
  
  xy <- rhrCheckData(xy, returnSP=FALSE)
  
  K <- 7  # we have 7 parameters

  ## nll
  nll <- function(theta, dat) {
    if (theta[7] < 0.001 | theta[7] > 0.99999) return(1e250)

    lh <- d2cbn(dat[ ,1], dat[, 2],
                m=theta[7], c1=theta[1:2],
                c2=theta[3:4], theta[5], theta[6])
    -1 * sum(log(ifelse(lh == 0, 0.00000001, lh)))
  }

  init <- initKmeans(xy)
  ll <- optim(init, nll, dat=xy)
  
  
  r1 <- data.frame(raster::rasterToPoints(trast))
  r1$density <- d2cbn(r1[, 1], r1[, 2], ll$par[1:2], ll$par[3:4], ll$par[5], ll$par[6], ll$par[7])
  ud <- raster::rasterFromXYZ(r1)

  AIC <- 2 * ll$value + 2 * K



  ## AICc
  

  res <- structure(
    list(
      model="Bimodal Circular", 
      args=list(
        xy=xy), 
      K=K,
      ud = ud, 
      LL=ll, 
      AIC=AIC, 
      AICc=AICc <- AIC + (2*K*(K+1)) / (nrow(xy) - K -1), 
      parameters=list(
        mean1=ll$par[1:2], 
        mean2=ll$par[3:4],
        sigma1=ll$par[5],
        sigma2=ll$par[6],
        pi=ll$par[7])), 
    class=c("RhrBiCirc", "RhrParamEst", "RhrProbEst", "RhrEst", "list"))
  return(invisible(res))

}


initKmeans <- function(dat) {
  init <- kmeans(dat, 2)
  m <- sum(init$cluster == 1)/nrow(dat)
  sd1 <- mean(apply(dat[init$cluster == 1, ], 2, sd))
  sd2 <- mean(apply(dat[init$cluster == 2, ], 2, sd))
  c(init$centers[1, ], init$centers[2, ], sd1, sd2, m)
}

d2cbn <- function(x, y, c1, c2, sigma1, sigma2, m) {
  rho <- 0
  p1 <- 1 / (2 * pi * sigma1 * sigma1 * sqrt(1 - rho^2))
  p2 <- -1 / (2 * (1 - rho^2))
  p3 <- ((x - c1[1]) / sigma1)^2 + ((y - c1[2]) / sigma1)^2
  p4 <- 2 * rho * ((x - c1[1]) / sigma1) * ((y - c1[2]) / sigma1)  ## if error check brackets here

  d1 <- (p1 * exp(p2 * (p3 - p4))) * m

  p1 <- 1 / (2 * pi * sigma2 * sigma2 * sqrt(1 - rho^2))
  p2 <- -1 / (2 * (1 - rho^2))
  p3 <- ((x - c2[1]) / sigma2)^2 + ((y - c2[2]) / sigma2)^2
  p4 <- 2 * rho * ((x - c2[1]) / sigma2) * ((y - c2[2]) / sigma2)

  d2 <- (p1 * exp(p2 * (p3 - p4))) * (1 - m)
  d <- d1 + d2

  ifelse(d == 0, 0.0000000000000001, d)
  
}



#' @export
#' @rdname parametric_homeranges
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


#' @export
rhrUD.RhrParamEst <- function(x, ...) {
  x$ud
}

#' @export
rhrCUD.RhrParamEst <- function(x, ...) {
  r1 <- rhrUD(x)
  rhrUD2CUD(r1)
}

#' @export
rhrIsopleths.RhrParamEst <- function(x, levels=NULL, ...) {
  
  if (is.null(levels)) {
    levels <- rhrLevels(x)
  } 
  cud <- rhrCUD(x)
  rhrCUD2Isopleths(cud, levels)
}

#' @export
rhrArea.RhrParamEst <- function(x, levels=95, ...) {
  as.data.frame(rhrIsopleths(x, levels))
}

#' @export
rhrHasUD.RhrParamEst <- function(x, ...) {
  TRUE
}

#' @export
rhrData.RhrParamEst <- function(x, spatial=FALSE, ...) {
  xx <- rhrCheckData(x$args$xy, returnSP=spatial)
}

#' @export
plot.RhrParamEst <- function(x, levels=95, ...) {
  ud <- rhrUD(x)
  iso <- rhrIsopleths(x, levels)
  plotRaster(ud)
  sp::plot(iso, add=TRUE)
}
