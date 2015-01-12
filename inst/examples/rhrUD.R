library(rhr)
data(datSH)

## For KDE
kde <- rhrKDE(datSH[, 2:3])
ud <- rhrUD(kde)
plot(rhrUD(kde))


## For unimodal normal
un <- rhrUniNorm(datSH[, 2:3])
ud <- rhrUD(un)
plot(ud)
