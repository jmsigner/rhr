set.seed(123)
x <- rnorm(500)
y <- rnorm(500)

msd <- rhr:::rhrBaseMSD(x, y)
