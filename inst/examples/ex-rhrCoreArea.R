library(rhr)
data(datSH)
hr <- rhrKDE(datSH[, 2:3])
#caIso <- rhrCoreArea(hr)

#plot(hr)
#plot(rhrIsopleths(hr, levels=caIso$iso), border="red", add=TRUE)
