##' Wrapper around \code{readLines} to read data with different field separator and to do some checking, mainly used for the GUI
##'
##' some details
##' @title read separator delimited text files
##' @param file character; the file name
##' @param sep character; the separator
##' @param skip numeric; the number of lines to skip
##' @param hasHeader logical; indicates whether or not a header is presence
##' @param sepDec character; indicating the decimal separator
##' @param ... passed to scan
##' @return \code{data.frame}
##' @export
##' @author Johannes Signer
rhrReadData <- function(file, sep=",", skip=0, hasHeader=TRUE, sepDec=".", stringsAsFactors=FALSE, ...) {
  
  ## Debugging
  if (FALSE) {
    file <- file.path(tempdir(), "test.txt")
    data(datSH)
    write.table(datSH, file, row.names=FALSE, col.names=TRUE, sep=",")
    sep <- ","
    skip <- 0
    hasHeader <- TRUE
    sepDec <- "."
  }

  if (!sep %in% c("|", "\\|", ";", ",", "\t")) {
    stop("rhrReadData: unknown field separator")
  }

  ## Account for pipe as sep
  if (sep == "|") {
    sep <- "\\|"
  }

  if (!sepDec %in% c(".", ",")) {
    stop("rhrReadData: unknown decimal separator")
  }

  if (file.exists(file)) {
    fileRaw <- read.table(file, header=hasHeader, dec=sepDec, sep=sep,
                          stringsAsFactors=FALSE, ...)
  } else {
    stop("rhrReadData: file does not exist")
  }


  if (skip >= nrow(fileRaw)) {
    stop("rhrReadData: you want to skip more lines that the data has")
  }

  if (!is.logical(hasHeader)) {
    stop("rhrReadData: hasHeader is expected to be logical")
  }

  if (!is.numeric(skip)) {
    stop("rhrReadData: skip is expected to be numeric")
  }
    
  if (skip < 0) {
    stop("rhrReadData: skip is expected to be positive")
  }

  ## discard lines to be skipped
  if (skip != 0) {
    fileRaw <- tail(fileRaw, -skip)
  }

  invisible(structure(fileRaw, class=c("RhrDat", "data.frame")))
}
