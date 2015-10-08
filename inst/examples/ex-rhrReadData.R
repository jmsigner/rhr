## Generate data
tmpDir <- tempdir()
dat <- data.frame(x=runif(1000),
                  y=runif(1000))

write.csv(dat, file.path(tmpDir, "test.csv"))

## Read data
dat <- rhrReadData(file.path(tmpDir, "test.csv"), sep=",")
