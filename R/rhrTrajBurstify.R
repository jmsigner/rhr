#' Splits a trajectory into bursts.
#'
#' Splits a regular trajectory into burts (i.e., sequence of relocations with no missing spatial information). 
#'
#' @param x Object of class \code{RhrTrajSTR}
#' @export

rhrTrajBurstify <- function(x, ...) {
  UseMethod("rhrTrajBurstify", x)
}

##' @export
rhrTrajBurstify.RhrTrajSTR <- function(x, minN = 5) {
  att <- rhrTrajAttribute(x)
  loc <- rhrTrajLocations(x)
  grp <- with(rle(att$rhrTrajSpace), rep(seq_along(lengths),lengths))
  grpSP <- grp[att$rhrTrajSpace]
  att <- split(att, grp)
  att <- att[which(names(att) %in% grpSP)]
  att <- att[which(sapply(att, nrow) >= minN)]
  loc <- split(loc, grpSP)
  loc <- loc[which(names(loc) %in% names(att))]
  loc <- lapply(loc, function(x) cbind(data.frame(x@sp), rhrTrajTime = time(x@time)))
  lapply(1:length(loc), function(i) rhr:::rhrTrajConstructorSTR(loc[[i]], att[[i]]))
}
