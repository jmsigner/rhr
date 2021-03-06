% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhrAsymptote.R
\name{rhrAsymptote}
\alias{rhrAsymptote}
\title{Asymptote of a home-range estimate}
\usage{
rhrAsymptote(x, ns = seq(nrow(rhrData(x))/10, nrow(rhrData(x)), length.out =
  10), nrep = 10, tolTotArea = 0.05, nTimes = 5,
  sampling = "sequential")
}
\arguments{
\item{x}{Object of class RhrEst, the home-range estimate for which the asymptote is calculated.}

\item{ns}{Numeric vector, the sample sizes for which the bootstrap is performed.}

\item{nrep}{Numeric value, number of bootstrap replicates for each sample size.}

\item{tolTotArea}{Numeric value (greater than 0 and smaller than 1), tolerance to the total area (that is the area using all relocations).}

\item{nTimes}{Numeric value, number of times the confidence interval is required to be within tolerated total area}

\item{sampling}{Character value, either 'random' or 'sequential'. See below for details.}
}
\value{
An object of class \code{RhrHRAsymptote}
}
\description{
\code{rhrAsymptote} returns the sample size at which a home-range is reached, if it present.
}
\details{
Bootstrapped home-range sizes are calculated for different sample sizes. Starting
from \code{ns[1]} relocations until a maximum sample size \code{ns[length(ns)]}
points. Home-range sizes are plotted against the sample sizes.
Laver (2005, 2005) suggested to use the following cutoff value: the number
of locations at which estimates of the 95 \% confidence interval of the
bootstrapped home-range area is within a specified percentage of the
total home range area (that is the area of the home range with all
relocations) for at least \code{nTimes} times. Harris 1990 suggested to use random
sampling for discontinuous radio tracking data and sequential sampling for
continuous radio tracking data.
}
\examples{

data(datSH)

## With MCP
\dontrun{
mcp <- rhrMCP(datSH[, 2:3])
mcpA <- rhrAsymptote(mcp)
plot(mcpA)

# maybe increase ns
# how many samples do we have?
nrow(datSH)
ns <- seq(20, nrow(datSH), 10)
mcpA <- rhrAsymptote(mcp, ns = ns)
plot(mcpA)
mcpA

# An asymptote is reached, but it seems that there is more structure in the
# data
# Lets have a look at the dates when the fixes were recorded
library(lubridate)
datSH$day <- ymd(datSH$day)
table(year(datSH$day))
# Lets only look at fixes from the first year
d1 <- datSH[year(datSH$day) == 2008, ]
nrow(d1)
ns <- seq(20, nrow(d1), 10)
mcp <- rhrMCP(d1[, 2:3])
mcpA <- rhrAsymptote(mcp, ns = ns)
plot(mcpA)
mcpA
}
}
\references{
Harris, S., et al. "Home-range analysis using radio-tracking data-a review of problems and techniques particularly as applied to the study of mammals." Mammal review 20.2-3 (1990): 97-123.

Peter N Laver. Cheetah of the serengeti plains: a home range analysis. Master's thesis, Virginia Polytechnic Institute and State University, 2005

Peter N. Laver and Marcella J. Kelly. A critical review of home range studies. The Journal of Wildlife Management, 72(1):290-298, 2008
}

