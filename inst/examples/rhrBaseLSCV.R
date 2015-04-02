set.seed(123)
x <- rnorm(500)
y <- rnorm(500)
h <- seq(0.1, 5, 0.1)
s <- rhrBaseLSCV(x, y, h)
h[which.min(s)]
