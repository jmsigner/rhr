library(rhr)
data(datSH)

rhrHref(datSH[, 2:3])
rhrHref(datSH[, 2:3], rescale = "unitvar")

kde1 <- rhrKDE(datSH[, 2:3], h = rhrHref(datSH[, 2:3])$h)
kde2 <- rhrKDE(datSH[, 2:3], h = rhrHref(datSH[, 2:3], "unitvar")$h)
kde3 <- rhrKDE(datSH[, 2:3], h = rhrHref(datSH[, 2:3], "xvar")$h)

lapply(list(kde1, kde2, kde3), rhrArea)
