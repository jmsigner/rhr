set.seed(123)

## Standard Walk
walk <- rhrOU(n = 5000)
plot(walk)

## Adjust pull back
walk <- rhrOU(n = 5000, A = matrix(c(0.01, 0, 0, 0.01), 2))
plot(walk)

## Effect of sigma: not only the scale of x and y changes
set.seed(123)
walk <- rhrOU(n = 5000, A = matrix(c(0.01, 0, 0, 0.01), 2), sigma = 1)
plot(walk)

set.seed(123)
walk <- rhrOU(n = 5000, A = matrix(c(0.01, 0, 0, 0.01), 2), sigma = 100)
plot(walk)


## Effect of xy0
set.seed(123)
walk <- rhrOU(n = 5000, A = matrix(c(0.01, 0, 0, 0.01), 2), sigma = 1, xy0 = c(50, 50))
plot(walk)
