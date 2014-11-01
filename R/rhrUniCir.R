rhrUniCir <- function(xy) {
  xy <- rhrCheckData(xy, returnSP=FALSE)

  ## estimate parameters
  ll <- function(xy, par) -sum(log(dbexppm(xy, par[1], par[2], par[3], par[4])))

  ## AIC
  phat <- optim(par=c(mean(xy[, 1]), mean(xy[, 2]), 10, 20), xy=xy, fn=ll, method="L-BFGS-B", lower=c(min(xy[, 1]), min(xy[, 1]), 0, 0))

  phat <- optim(par=c(mean(xy[, 1]), mean(xy[, 2]), 10, 20), xy=xy, fn=ll, method="L-BFGS-B", lower=c(-Inf, -Inf, 0, 0))$par
  ll <- sum(log(dbexppm(xy[, 1:2], phat[1], phat[2], c=phat[3], a=phat[4])))

  ## AIC
  K <- 4
  AIC <- -2 * ll + 2 * K

  ## AICc
  AICc <- AIC + (2*K*(K+1)) / (nrow(xy) - K -1)

  res <- structure(
    list(
    model="Unimodal Circular", 
    K=K,
    LL=ll, 
    AIC=AIC, 
    AICc=AICc, 
    parameters=list(
      mean=phat[1:2],
      c=phat[3],
      a=phat[4])),
    class=c("RhrUniNorm", "RhrEst", "list"))
  return(invisible(res))
}

dbexppm <- function(xy, mux, muy, c, a) {
  (2 / (c * 2 * pi * a^2 * gamma(c))) * exp(-(sqrt((xy[, 1] - mux)^2 + (xy[, 2] - muy)^2)/a)^(2/c))
}
