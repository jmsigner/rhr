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




