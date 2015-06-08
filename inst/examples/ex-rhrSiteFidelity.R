set.seed(123)


## Simulated data

# Simulate a random walk.
a <- rhrRW(n = 1000)
plot(a)

# Calcualte site fidelity
sf <- rhrSiteFidelity(a, n = 200)

# For MSD and LI the observed data do not differ significantely from random permutations. 
sf
plot(sf)

# Simulate trajectory as Ornstein-Uhlenbeck process
a <- rhrOU(n = 10000, A = matrix(c(0.1, 0, 0, 0.1), 2))
plot(a)
sf <- rhrSiteFidelity(a, n = 200)

# For MSD and LI the observed data differ significantely from random permutations. 
sf
plot(sf)

## real data
data(datSH)
res <- rhrSiteFidelity(datSH[, 2:3], n = 200)
res
plot(res)
