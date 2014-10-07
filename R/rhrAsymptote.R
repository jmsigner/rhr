## ============================================================================== ##  
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
## ============================================================================== ##

##' Calculate home rnage asymptote for an object of class RhrHREstimator
##' 
##' calculate home range asymptote
##' @param x RhrHREstimator object
##' @param ns numeric vector of the number of samples to be taken at each step
##' @param nrep the number of replicates for each sample size
##' @param tolTotArea tolerance to the total area (that is the area using all points)
##' @param nTimes the number of times the confidence interval is required to be within tolerated total area
##' @param sampling this should be either random or sequential. See notes for details.
##' @note Bootstrapped home ranges are calculated for different sample sizes. Starting from very few samples until the sample size approaches the total number of points. Home range sizes are then plotted against the sample sizes. Laver (2005, 2005) suggested to use the following cutoff value: the number of location estimates at which the 95 \% confidence interval of the bootstrapped home-range estimates is within a specified percentage of the total home range size (that is the size of the home range with all relocations) for at least n times. Harris 1990 suggested to use random sampling for discontinuous radio tracking data and sequential sampling for continuous radio tracking data. 
##' @return An object of class \code{RhrHRAsymptote}
##' @references Peter N Laver. Cheetah of the serengeti plains: a home range analysis. Master's thesis, Virginia Polytechnic Institute and State University, 2005
##' @references Peter N. Laver and Marcella J. Kelly. A critical review of home range studies. The Journal of Wildlife Management, 72(1):290-298, 2008
##' @export
##' @examples
##' \dontrun{
##' ## calculate home range asymptote for kernel density estimation
##' ## first calculate the home range
##' hr <- rhrKDE(datSH[, 2:3], h="href")
##' hra <- rhrAsymptote(hr)
##' plot(hra)
##' }


rhrAsymptote <- function(x, ns=seq(100, nrow(rhrData(x)), 500), nrep=10, tolTotArea=0.05, nTimes=5, sampling="sequential") {

  ## Debug only
  if (FALSE) {
    library(rhr)
    data(datSH)
    x <- rhrMCP(datSH[, 2:3], levels=c(50, 95))
    ns <- seq(100, nrow(x$args$xy), 100)
    nrep <- 10
    tolTotArea <- 0.05
    nTimes <- 5
    sampling <- "sequential"
    x <- rhrAsymptote(x)
              
  }

  ## Input checks
  if (!is(x, "RhrEst")) {
    stop("x is not of class RhrEst")
  }

  if (!is(ns, "numeric")) {
    stop("ns is not numeric")
  }

  if (!is(nrep, "numeric")) {
    stop("nrep is not numeric")
  }

  if (nrep <= 0) {
    stop("nrep should be >= 0")
  }

  if (!is(tolTotArea, "numeric")) {
    stop("tolTotArea is not numeric")
  }
  
  if (!is(nTimes, "numeric")) {
    stop("nTimes is not numeric")
  }

  if (!(tolTotArea > 0 & tolTotArea <= 1)) {
    stop("tolTatArea should be between 0 and 1")
  }

  if (nTimes <= 0) {
    stop("nTimes should be > 0")
  }

  if (!sampling %in% c("random", "sequential")) {
    stop("sampling should be either random or sequential")
  }

  if (max(ns) > nrow(rhrData(x))) {
    ns <- ns[ns <= nrow(rhrData(x))]
  }

  if (length(ns) <= 1) {
    stop("Requested sample size larger than the number of observation")
  }

  # Which estimator was used
  est <- NA
  est <- switch(class(x)[1], 
                "RhrMCP" = "rhrMCP",
                "RhrKDE" = "rhrKDE",
                "RhrLoCoH" = "rhrLoCoH")

  providedArgs <- x$args
  for (i in seq_along(providedArgs)) {
    if (is.character(providedArgs[[i]])) {
      providedArgs[[i]] <- shQuote(providedArgs[[i]])
    }
  }

  n <- nrow(rhrData(x))
  bb <- replicate(nrep,
                  do.call(rbind,
                          lapply(ns,
                                 function(nss) 
                                   cbind(ns=nss,
                                         rhrArea(
                                           do.call(est,
                                                   c(if (sampling == "sequential") list(xy=(xx <- providedArgs[[1]][1:nss, ])[sample(nrow(xx), replace=TRUE),])
                                                   else list(xy=providedArgs[[1]][sample(n, nss),]),
                                                     providedArgs[-1])))))),
                  simplify=FALSE)

  bb <- do.call(rbind, bb)
    
  
  totalA <- rhrArea(x)
  totalA$lower <- totalA$area * (1 - tolTotArea)
  totalA$upper <- totalA$area * (1 + tolTotArea)

      
  ## calculate confidence intervals
  confints <- do.call(rbind, lapply(split(bb, bb[, c("level", "ns")]), function(x) {
    tt <- quantile(x$area, probs=c(0.05, 0.95))
    data.frame(level=x$level[1], ns=x$ns[1], lower=tt[1], upper=tt[2])
  }))



  confints <- split(confints, confints$level)

  ## figure out when the Asymptote was reached at each level j
  ## The asymptote is reached when the both CI are inside the area tolerance for x times
  asymReached <- sapply(seq_along(confints), function(j) {
    ## check if is inside
    isInside <- rle((confints[[j]]$V1 >= totalA[j, "lower"])  &
                    (confints[[j]]$V2 <= totalA[j, "upper"]))

    whereInside <- with(isInside, which(lengths >= nTimes & values))[1]

    ## NA if all outside
    if (length(whereInside) > 0 & !is.na(whereInside)) {
      unique(ns)[sum(isInside$lengths[1:(whereInside-1)]) + nTimes]
    } else {
      NA
    }
  })

  asymReached <- data.frame(level=as.numeric(names(confints)),
                            ns=asymReached)

  ## plot
  out <- list(asymptote=asymReached, confints=do.call("rbind", confints), hrAreas=bb, call=match.call(),
              params=list(ns=ns, tolTotArea=tolTotArea), totalA=totalA, hrEstimator=x)
  class(out) <- "RhrHRAsymptote"
  out

}


##' print object of rhrHRAsymptote
##' 
##' @param x RhrHRAsymptote
##' @param ... none implemented
##' @method print RhrHRAsymptote
##' @export

print.RhrHRAsymptote <- function(x, ...) {

  cat(paste0("class                    : ", class(x)),
      paste0("asymptote calculated for : ", paste0(x$asymptote$level, collapse=",")),
      paste0("asypotote reached at     : ", paste0(x$asymptote$ns, collapse=",")),
      sep="\n")

}


##' plot for RhrHRAsymptote
##' 
##' generic plot for RhrHRAsymptote
##' @param x RhrHRAsymptote
##' @param draw indicates whether the plot should be drawn or not. If this is  \code{FALSE} a grob is returned.
##' @param ... none implemented
##' @method plot RhrHRAsymptote
##' @export

plot.RhrHRAsymptote <- function(x, ...) {

  ## Input checks
  ## to be completed
  cc <- reshape2::melt(x$confints, id=c("level", "ns"))

  ## totalA
  totalA <- x$totalA
  tolTotArea <- x$params$tolTotArea
  ns <- x$params$ns
  
  dd <- data.frame(xx=rep(unique(x$params$ns), length(unique(x$hrAreas$level))),
                   ymin=rep(totalA$lower, each=length(unique(ns))),
                   ymax=rep(totalA$upper, each=length(unique(ns))),
                   level=rep(totalA$level, each=length(unique(ns))))
  
  ## When were the asymtotes reaches?
  asymR <- x$asymptote
  asymR <- asymR[complete.cases(asymR), ]

  
  
  p <- ggplot(x$hrAreas, aes(x=ns, y=area, group=ns)) +
    geom_point(alpha=0.5) +  
      geom_ribbon(data=dd, aes(x=xx, ymin=ymin, ymax=ymax, group=NULL, y=NULL, alpha=0.4)) +
        geom_hline(aes(yintercept=area), linetype="dashed", data=totalA)  +
          geom_line(aes(x=ns, y=value, group=variable), alpha=0.5, data=reshape2::melt(x$confints, id.vars=c("level", "ns"))) 

  if (nrow(asymR) >= 1) {
    p <- p + geom_vline(aes(xintercept=ns), linetype="solid", colour="red", data=asymR) 
  }

  p <- p + facet_wrap(~level, ncol=2, scales="free_y") +
    theme_bw() +
      theme(legend.position="none") +
        labs(x="Number of points", y="Area")

  return(p)
}
