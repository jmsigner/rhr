
rhrPHR <- function(xy, models, AICcThreshold=50) {

  ## Some input validation
  xy <- rhrCheckData(xy, returnSP=FALSE)

  if (FALSE) {
    setwd("~/Dropbox/21_lit/supplements/horne2006")
    xy <- read.table("locations/BF03.txt", header=TRUE)
    head(xy)

    rhrPHR(xy, model=c("unicir"))
    rhrPHR(xy, model=c("unicir", "uninorm"))
    rhrPHR(xy, model=c("unicir", "uninorm", "binorm"))
    rhrPHR(xy, model=c("unicir", "uninorm", "bicir"))
    a <- rhrPHR(xy, model=c("unicir", "uninorm", "bicir", "binorm"))

    models <- c("unicir", "uninorm", "bicir", "binorm")
  }

  res <- list()
  res$est <- list()
  ## ============================================================================== ##  
  ## unimodal circular

  if ("unicir" %in% models) {
    res$est$unicir <- tryCatch(rhrUniCir(xy), error=function(e) e)
  }

  ## ============================================================================== ##  
  ## unimodal normal

  if ("uninorm" %in% models) {
    res$est$uninorm <- tryCatch(rhrUniNorm(xy), error=function(e) NULL)
  }

  ## ============================================================================== ##  
  ## bimodal circular

  if ("bicir" %in% models) {
    res$est$bicir <- tryCatch(rhrBiCir(xy), error=function(e) NULL)
  }

  ## ============================================================================== ##  
  ## bimodal normal

  if ("binorm" %in% models) {
    res$est$binorm <- tryCatch(rhrBiNorm(xy), error=function(e) NULL)
  }

  ov <- data.frame(Name=sapply(res$est, function(x) x$model),
                   K=sapply(res$est, function(x) x$K), 
                   n2LL=-2 * sapply(res$est, function(x) x$LL), 
                   AIC=sapply(res$est, function(x) x$AIC), 
                   AICc=sapply(res$est, function(x) x$AICc))
                     
  ov$dAIC <- ov$AIC - min(ov$AIC)
  ov$dAICc <- ov$AICc - min(ov$AICc)

  if (nrow(xy) > AICcThreshold) {
    ov <- ov[order(ov$dAICc), ]
  } else {
    ov <- ov[order(ov$dAIC), ]
  }
  return(list(res$est, ov))
}






