context("rhrReadData: test cases")

## Generate Data
library(rhr)
library(sp)
data(datSH)

write.table(datSH, file=file.path(tempdir(), "datSepComaHeader.txt"), sep=",", row.names=FALSE, col.names=TRUE)
write.table(datSH, file=file.path(tempdir(), "datSepPipeHeader.txt"), sep="\\|", row.names=FALSE, col.names=TRUE)
write.table(datSH, file=file.path(tempdir(), "datSepSemiColonHeader.txt"), sep=";", row.names=FALSE, col.names=TRUE)
write.table(datSH, file=file.path(tempdir(), "datSepTabHeader.txt"), sep="\t", row.names=FALSE, col.names=TRUE)

write.table(datSH, file=file.path(tempdir(), "datSepComaNoHeader.txt"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(datSH, file=file.path(tempdir(), "datSepPipeNoHeader.txt"), sep="\\|", row.names=FALSE, col.names=FALSE)
write.table(datSH, file=file.path(tempdir(), "datSepSemiColonNoHeader.txt"), sep=";", row.names=FALSE, col.names=FALSE)
write.table(datSH, file=file.path(tempdir(), "datSepTabNoHeader.txt"), sep="\t", row.names=FALSE, col.names=FALSE)


test_that("Missing filename is recognised", {
  expect_that(rhrReadData(), throws_error())
})

test_that("Non existing filename is recognised", {
  expect_that(rhrReadData(file.path(tempdir(), "foo.txt")), throws_error())
})


test_that("Separators work", {

  expect_that(nrow(rhrReadData(file.path(tempdir(), "datSepComaHeader.txt"))), is_equivalent_to(nrow(datSH)))
  expect_that(nrow(rhrReadData(file.path(tempdir(), "datSepComaNoHeader.txt"), hasHeader=FALSE)), is_equivalent_to(nrow(datSH)))
  expect_that(ncol(rhrReadData(file.path(tempdir(), "datSepComaHeader.txt"))), is_equivalent_to(ncol(datSH)))
  expect_that(ncol(rhrReadData(file.path(tempdir(), "datSepComaNoHeader.txt"), hasHeader=FALSE)), is_equivalent_to(ncol(datSH)))
  
  ##expect_that(nrow(rhrReadData(file.path(tempdir(), "datSepPipeHeader.txt"), sep="\\|")), is_equivalent_to(nrow(datSH)))
  ##expect_that(nrow(rhrReadData(file.path(tempdir(), "datSepPipeNoHeader.txt"), sep="|", hasHeader=FALSE)), is_equivalent_to(nrow(datSH)))
  ##expect_that(ncol(rhrReadData(file.path(tempdir(), "datSepPipeHeader.txt"), sep="|")), is_equivalent_to(ncol(datSH)))
  ##expect_that(ncol(rhrReadData(file.path(tempdir(), "datSepPipeNoHeader.txt"), sep="|", hasHeader=FALSE)), is_equivalent_to(ncol(datSH)))

  expect_that(nrow(rhrReadData(file.path(tempdir(), "datSepTabHeader.txt"), sep="\t")), is_equivalent_to(nrow(datSH)))
  expect_that(nrow(rhrReadData(file.path(tempdir(), "datSepTabNoHeader.txt"), sep="\t", hasHeader=FALSE)), is_equivalent_to(nrow(datSH)))
  expect_that(ncol(rhrReadData(file.path(tempdir(), "datSepTabHeader.txt"), sep="\t")), is_equivalent_to(ncol(datSH)))
  expect_that(ncol(rhrReadData(file.path(tempdir(), "datSepTabNoHeader.txt"), sep="\t", hasHeader=FALSE)), is_equivalent_to(ncol(datSH)))

  expect_that(nrow(rhrReadData(file.path(tempdir(), "datSepSemiColonHeader.txt"), sep=";")), is_equivalent_to(nrow(datSH)))
  expect_that(nrow(rhrReadData(file.path(tempdir(), "datSepSemiColonNoHeader.txt"), sep=";", hasHeader=FALSE)), is_equivalent_to(nrow(datSH)))
  expect_that(ncol(rhrReadData(file.path(tempdir(), "datSepSemiColonHeader.txt"), sep=";")), is_equivalent_to(ncol(datSH)))
  expect_that(ncol(rhrReadData(file.path(tempdir(), "datSepSemiColonNoHeader.txt"), sep=";", hasHeader=FALSE)), is_equivalent_to(ncol(datSH)))


})
