#' Obtain the AIC of a parametric home-range
#' 
#' Generic function to obtain Akaike's Information Criterion (AIC) for parametric home-ranges. This is currently only possible for parametric home-ranges.
#' 
#' @param x The home-range estimate.
#' @param AICc Logical value, indicating if the AICc should be returned instead of the AIC.
#' @template dots
#' @seealso \code{\link[stats]{AIC}}
#' @export
#' @examples 
#' # Simulated data
#' set.seed(123)
#' dat <- data.frame(x = c(rnorm(100), rnorm(100, 20)), 
#' y = c(rnorm(100), rnorm(100, 20)))
#' uniCir <- rhrUniCirc(dat)
#' AIC(uniCir)
#' AIC(uniCir, AICc = TRUE)
#' biCir <- rhrBiCirc(dat)
#' AIC(biCir)
#' AIC(biCir, AICc = TRUE)
#' uniNorm <- rhrUniNorm(dat)
#' AIC(uniNorm)
#' AIC(uniNorm, AICc = TRUE)
#' biNorm <- rhrBiNorm(dat)
#' AIC(biNorm)
#' AIC(biNorm, AICc = TRUE)

rhrAIC <- function(x, AICc = FALSE, ...) {
  UseMethod("rhrAIC", x)
}

#' @export
rhrAIC.default <- function (x , ...) {
  paste0 ("rhrAIC is not defined for object of class", class(x))
}

#' @export
rhrAIC.RhrParamEst <- function(x, AICc = FALSE, ...) {
  if (AICc) {
    x$AICc
  } else {
    x$AIC
  }
}
