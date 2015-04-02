## sequence of 10 seconds
x <- seq(0, 3600, by = 60)
x[rhrBaseIntervalSubset(x, 600)]

## make the sequence irregular
x <- sort(sample(x, 40))
x[rhrBaseIntervalSubset(x, 600)]
