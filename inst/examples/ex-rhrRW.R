set.seed(123)

## All steps with equal sigma
walk <- rhrRW()
plot(walk)

hist(rhrSegments(walk)$distance)

## Three different sigmas
walk <- rhrRW(n = 3000, sigma = rep(c(1, 5, 1), each = 1000))
plot(walk)
hist(rhrSegments(walk[1:1000, ])$distance)
hist(rhrSegments(walk[1001:2000, ])$distance)
hist(rhrSegments(walk[2001:3000, ])$distance)

## Sigma varies over time following a sine curve
sigma <- sin(seq(0, pi, length.out = 1000)) * 2
plot(sigma, type = "l")
walk <- rhrRW(n = 1000, sigma = sigma)
plot(walk)

## Does home-range size change over time as well?
# b <- rhrSplit(walk, rep(1:5, each = 200))
# plot(sapply(b, function(x) rhrArea(rhrMCP(x))$area), 
#     xlab = "time", ylab = "Home-range size")

## Time steps do not have to be equal
times <- lubridate::now() + lubridate::minutes(cumsum(rpois(1000, 10)))
walk <- rhrRW(n = 1000, time = times)
plot(walk)
hist(rhrSegments(walk)$duration)

