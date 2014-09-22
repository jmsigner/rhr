##' @export
rhrBiNorm <- function(xy, trast=NULL, proj4string=NA) {

  if (FALSE) {
    foo <- rhrBiNorm(datSH[1:1500, 2:3])
    plot(rhrUD(foo))
    plot(rhrCUD(foo))
    plot(rhrIsopleths(foo, levels=c(10, 30, 50, 90, 99)), add=T)
  }

  ## Capture input arguments
  args <- as.list(environment())
  call <- match.call()

  ## check input 
  xy <- rhrCheckData(xy, returnSP=FALSE)
  projString <- rhrProjString(xy, projString=proj4string)


  hats <- mvnormalmixEM(xy[, 1:2], k=2)

  if (is.null(trast)) {
    trast <- rhrRasterFromExt(rhrExtFromPoints(xy, extendRange=0.2), nrow=100, res=NULL)
  }

  r1 <- data.frame(rasterToPoints(trast))
  r1$density <- d2mvnorm(r1[, 1:2], m=hats$lambda[1], 
                         mu1=hats$mu[[1]], sig1=hats$sigma[[1]], mu2=hats$mu[[2]], sig2=hats$sigma[[2]])

  ud <- rasterFromXYZ(r1)

  ## log likelihood
  ll <- hats$loglik

  ## AIC
  K <- 11
  AIC <- -2 * ll + 2 * K

  ## AICc
  AICc <- AIC + (2*K*(K+1)) / (nrow(xy) - K -1)

  ## project
  proj4string(ud) <- projString

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
    class=c("RhrBiNorm", "RhrEst", "list"))
  return(invisible(res))
}

d2mvnorm <- function(xy, m, mu1, sig1, mu2, sig2) {
  m * mvtnorm::dmvnorm(xy, mu1, sig1) + (1 - m) * mvtnorm::dmvnorm(xy, mu2, sig2)
}

##' @export
rhrUD.RhrBiNorm <- function(x, ...) {
  x$ud
}



##' @export
rhrCUD.RhrBiNorm <- function(x, ...) {
  r1 <- rhrUD(x)
  rhrUD2CUD(r1)
}

##' @export
rhrIsopleths.RhrBiNorm <- function(x, levels=95, ...) {
  cud <- rhrCUD(x)
  rhrCUD2Isopleths(cud, levels)
}

##' @export
rhrArea.RhrBiNorm <- function(x, levels=95, ...) {
  as.data.frame(rhrIsopleths(x, levels))
}

##' @export
plot.RhrBiNorm <- function(x, levels=95, ...) {
  cud <- rhrCUD(x)
  iso <- rhrIsopleths(x, levels)
  plot(cud)
  plot(iso, add=TRUE)
}
