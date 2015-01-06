##' Estimate bandwidth with likelihood cross validation
##'
##' Likelihood Cross Validation
##'
##' @param xy data.frame with two columns: x and y coordinates
#
##' @return \code{vector} of length two
##' 
##' @author Johannes Signer 
##' @examples
##' data(datSH)
##' rhrHpi(datSH[, 2:3])

rhrHcv <- function(xy) {

  ## Some input validation
  xy <- rhrCheckData(xy, returnSP=FALSE)

  

  
}
